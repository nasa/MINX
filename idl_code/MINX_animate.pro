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

; cw_animate.pro,v 1.36 2002/02/06 21:45:42 scottm
;
; Copyright (c) 1992-2002, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
; Extensively modified by D.Nelson, 2005-2015
;
; --------------------------------------------------------------------------
; Hierarchy of widgets/windows in MINX animation option (window ID: "uname")
;
; wVeryTop : "VERY_TOP_BASE"
;    wAnimateBase : "ANIMATE_BASE"
;       wControlBaseTop : "CONTROL_BASE_TOP"
;          various widgets
;       wTopWorkBase : "TOP_WORK_BASE" - contains state structure 
;          wDrawBase : "DRAW_BASE"
;             wDrawWindow : "DRAW_WINDOW" - where all images are shown
;       wControlBaseBot : "CONTROL_BASE_BOT"
;          various widgets
; --------------------------------------------------------------------------
;
; NAME:
;	CW_ANIMATE
;
; PURPOSE:
;	This widget displays an animated sequence of images using X-windows
;	 Pixmaps. This is a compound widget, based on the XINTERANIMATE 
;	 procedure, with the following advantages:
;          - It can be included in other applications.
;	   - Multiple copies can be run simultaneously.
;
;	The speed and direction of the display can be adjusted using the
;       widget interface.
;
; CATEGORY:
;	Image display, compound widgets.
;
; CALLING SEQUENCE:
;	To initially create:
;		widget = CW_ANIMATE(PARENT, SIZEX, SIZEY, NFRAMES)
;
;	To reinitialize when another animation is loaded:
;		CW_ANIMATE_INITME, ANIMATEBASE, SIZEX, SIZEY, NFRAMES
;
;	To load a single image:
;		CW_ANIMATE_LOAD, WIDGET, IMAGE = IMAGE, FRAME = FRAME_INDEX
;
;	To load a single image that is already displayed in existing window:
;
;		CW_ANIMATE_LOAD, WIDGET, FRAME = FRAME_INDEX, $
;			WINDOW = [WINDOW_NUMBER [, X0, Y0, SX, SY]]
;
;	(This technique is much faster than reading back from the window.)
;
;	To display the animation after all the images have been loaded:
;
;		CW_ANIMATE, WIDGET [, RATE]
;
;	To get a copy of the vector of Pixmaps being used by the widget.
;	If this routine is called, the widget does not destroy the pixmaps
;	when it is destroyed. The user can then provide them to a later
;	call to CW_ANIMATE to re-use them while skipping the Pixmap creation
;	and rendering step:
;
;		CW_ANIMATE_GETP, widget, PIXMAPS
;
; INPUTS:
;   CW_ANIMATE:
;		PARENT:	 The ID of the parent widget.
;		SIZEX:	 The width of the displayed image.
;		SIZEY:	 The height of the displayed image.
;		NFRAMES: The number of frames in the animation sequence.
;
;   CW_ANIMATE_INITME:
;		ANIMATEBASE: The ID of the base animation widget.
;		SIZEX:	 The width of the displayed image.
;		SIZEY:	 The height of the displayed image.
;		NFRAMES: The number of frames in the animation sequence.
;
;   CW_ANIMATE_LOAD:
;		WIDGET:	 The ID of the widget (as created with CW_ANIMATE)
;			 into which the image should be loaded.
;
;   CW_ANIMATE_RUN:
;		WIDGET:	 The ID of the widget (as created with CW_ANIMATE)
;			 into which the image should be loaded.
;		RATE:	 A value between 0 and 100 that represents the
;			 speed of the animation as a percentage of the
;			 maximum display rate. The fastest animation has
;			 a value of 100 and the slowest  has a value of 0.
;			 The default animation rate is 100.
;       STOP:    If this keyword is set, the animation is stopped.
;       NFRAMES: Specifies the number of frames to animate, must
;                        <= the number specified in CW_ANIMATE().
;
; KEYWORD PARAMETERS:
;   	CW_ANIMATE:
;		PIXMAPS: This keyword provides the new widget with a vector
;			 of pre-existing pixmap (off screen window) IDs.
;			 This vector is usually obtained from a call to
;			 CW_ANIMATE_GETP applied to a previous animation
;			 widget.
;		UVALUE:  A user supplied value to be stored in the widget's
;			 user value field.
;               UNAME:   A user supplied string name to be stored in the
;                        widget's user name field.
;               NO_KILL: If NOT set, an "End Animation" button is added to
;			 the animation base.  If set, button is not added.
;		OPEN_FUNC: A user supplied string that specifies a callback
;			 function name. When a value is specified for this
;			 keyword, an "Open..." pushbutton is added to the
;			 window.  When the "Open..." pushbutton is clicked
;			 the OPEN_FUNC function is called to load new
;			 animation data.
;		INFO_FILE: A filename containing text to be displayed by
;                        XDISPLAYFILE when user selects the help button.
;
;   	CW_ANIMATE_INITME:
;		PIXMAPS: This keyword provides the new widget with a vector
;			 of pre-existing pixmap (off screen window) IDs.
;			 This vector is usually obtained from a call to
;			 CW_ANIMATE_GETP applied to a previous animation
;			 widget.
;
;   	CW_ANIMATE_LOAD:
;		CYCLE:   If set, cycle. Normally, frames are displayed
;			 going either forward or backwards. If CYCLE is
;			 set, reverse direction after the last frame in
;			 either direction is displayed.
;		FRAME: 	 The frame number to be loaded. This is a value
;			 between 0 and NFRAMES. If not supplied, frame 0
;		  	 is loaded.
;		IMAGE:   The image to be loaded.
;		ORDER:   Set this keyword to display images from the top
;			 down instead of the default bottom up. This keyword
;			 is only used when loading images with the IMAGE
;			 keyword.
;		WINDOW:  When this keyword is specified, an image is copied
;			 from an existing window to the animation pixmap.
;			 When using X windows, this technique is much faster
;			 than reading from the display and then loading with
;			 the IMAGE keyword.
;
;			 The value of this parameter is either an IDL window
;			 number (in which case the entire window is copied),
;			 or a vector containing the window index and the
;			 rectangular bounds of the area to be copied. For
;			 example:
;			 WINDOW = [Window_Number, X0, Y0, Sx, Sy]
;
;      		XOFFSET: The horizontal offset, in pixels from the left of
;			 the frame, of the image in the destination window.
;
;      		YOFFSET: The vertical offset, in pixels from the bottom of
;			 the frame, of the image in the destination window.
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	If the widget is realized before calls to CW_ANIMATE_LOAD, frames
;	are displayed as they are loaded. This provides the user with an
;	indication of how things are progressing.
;
;	When the widget is destroyed, it destroys the pixmaps used in the
;	animation, unless they were previously obtained via CW_ANIMATE_GETP
;       and the KILL_ANYWAY keyword was not set.
;
;	The only event returned by this widget indicates that the user
;	has pressed the DONE button. The parent application should use
;	this as a signal to kill the animation widget via WIDGET_CONTROL.
;
; RESTRICTIONS:
;	If more than one animation widget is running at a time, they
;	will fight for resources and run slower.
;
; PROCEDURE:
;	When initialized, this procedure creates pixmaps containing the
;	frames of the animation sequence. Once the images are loaded,
;	they are displayed by copying the images from the pixmap or buffer
;	to the visible draw widget.
;
; EXAMPLE:
;	Assume the following event handler procedure exists:
;		PRO EHANDLER, EV
;		  WIDGET_CONTROL, /DESTROY, EV.TOP
;		end
;
;	Enter the following commands to open the file ABNORM.DAT (a series
;	of images of a human heart) and load the images it contains into
;	an array H:
;
;		OPENR, 1, FILEPATH('abnorm.dat', SUBDIR = 'images')
;		H = BYTARR(64, 64, 16)
;		READU, 1, H
;		FREE_LUN, 1
;		H = REBIN(H, 128, 128, 16)
;
;	Create an instance of the animation widget at load the frames:
;
;		base = widget_base()
;		animate = CW_ANIMATE(base, 128, 128, 16)
;		WIDGET_CONTROL, /REALIZE, base
;		for i=0,15 do CW_ANIMATE_LOAD,animate,FRAME=i,IMAGE=H[*,*,I]
;
;	Start the animation:
;
;		CW_ANIMATE_RUN, animate
;		XMANAGER, "CW_ANIMATE Demo",base,EVENT_HANDLER = "EHANDLER"
;
;	Pressing the DONE button kills the application.
;
; MODIFICATION HISTORY:
;	AB, June 1992		Heavily based on the XINTERANIMATE procedure.
;	SR, September 1992	Fixed a problem when a paused animation's
;				frame selection was moved and the resulting
;				frame change ended up in another animation.
;	SR, November  1992	Fixed a problem when single paused animation
;				would fail when the frame selection slider
;				event tried to set do a bad drawing window.
;	DMS/AB, March, 1993	Got rid of state caching. Got rid of
;				XMANAGER background tasks in favor of new
;				"WIDGET_CONTROL,timer=" feature.
;	ACY, October 1993	Set RETAIN=2 for draw widget to prevent
;				clipping by an overlapping window when loading frames.
;
;   DMS, Dec, 1993   Added STOP and NFRAMES keywords to CW_ANIMATE_RUN.
;                    Added KILL_ANYWAY keyword to CW_ANIMATE_GETP.
;   WSO, Jan, 1995   Added OPEN_FUNC keyword and updated UI.
;   ACY, Jan, 1997   Added INFO_FILE keyword to allow user-supplied
;                    files for help text
;   JLP, Jan, 2000   Allow TrueColor images as input to CW_ANIMATE_LOAD.

;***************************************************************************
PRO SetMyBitmapButtons, state
;***************************************************************************

  compile_opt IDL2, hidden

  COMMON bitmap_btns, reversebutton, blk_reversebutton, pausebutton, $
                      blk_pausebutton, playbutton, blk_playbutton, $
                      cycleForwardBtn, blk_cycleForwardBtn

  WIDGET_CONTROL, state.currentAction, SET_VALUE = state.currentBitmap

  IF state.framedelta EQ 0 THEN BEGIN  ; paused
     WIDGET_CONTROL, state.wPauseButton, SET_VALUE = blk_pausebutton
     state.currentAction = state.wPauseButton
     state.currentBitmap = pausebutton
  ENDIF ELSE BEGIN
     IF state.framedelta GT 0 THEN BEGIN  ; animating forward
        IF state.cycle THEN BEGIN
           WIDGET_CONTROL, state.wCyclePlayButton, $
                           SET_VALUE = blk_cycleForwardBtn
           state.currentAction = state.wCyclePlayButton
           state.currentBitmap = cycleForwardBtn
        ENDIF ELSE BEGIN
           WIDGET_CONTROL, state.wPlayButton, $
                           SET_VALUE = blk_playbutton
           state.currentAction = state.wPlayButton
           state.currentBitmap = playbutton
        ENDELSE
     ENDIF ELSE BEGIN                       ; animating backwards
        WIDGET_CONTROL, state.wReversePlayButton, $
                           SET_VALUE = blk_reversebutton
        state.currentAction = state.wReversePlayButton
        state.currentBitmap = reversebutton
     ENDELSE
  ENDELSE

END  ;  SetMyBitmapButtons

;***************************************************************************
PRO CW_ANIMATE_CLEAN, widget
;***************************************************************************
; When the widget dies, clean up here. Widget is the ID of the *child*
; actually holding the state in its UVALUE.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE, hidden

     ; kills state stored in widget
   WIDGET_CONTROL, widget, GET_UVALUE = state, /NO_COPY

   IF (N_ELEMENTS(state) GT 0) THEN BEGIN
      IF (state.nokill_pixmaps EQ 0) THEN BEGIN
         pwin = *state.pwinHdl
         FOR i=0, N_ELEMENTS(pwin)-1 DO BEGIN
            SafeWDELETE, pwin[i], didit
            pwin[i] = -1
         ENDFOR
         PTR_FREE, state.pwinHdl
      ENDIF
        ; Restore the state
      WIDGET_CONTROL, widget, SET_UVALUE = state, /NO_COPY
   ENDIF

END  ;  CW_ANIMATE_CLEAN

;***************************************************************************
FUNCTION CW_ANIMATE_EVNT, event
;***************************************************************************
; Event handler for animation window (wTopWorkBase).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE, hidden
  
COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON save_product, ProductList

; Retrieve the structure from the child that contains the sub ids

wAnimateBase = event.handler
wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')

;This kills the old uvalue
WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY
ret = 0

NCAMS = state.nframes - 1

state.mousebtnstatus = 0
    
;------------------------------------------------------------------------
; If mouse enters or leaves the draw window, give it the input focus
; so keyboard events are received.
;------------------------------------------------------------------------
    
IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING') THEN $
   WIDGET_CONTROL, state.wDrawWindow, /INPUT_FOCUS

;------------------------------------------------------------------------
; Keyboard and mouse events begin here.
;------------------------------------------------------------------------

IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

   ;---------------------------------------------------------------------
   ; Check if an accelerator key was pressed. If event.modifiers has the
   ; 2 bit set, the control key was pressed along with another key.
   ;--------------------------------------------------------------------- 

   IF ((event.type EQ 5) AND ((event.modifiers AND 2) NE 0) AND $
       event.press EQ 0) THEN BEGIN

      char = STRING(event.ch + 64B)
   
      ;------------------------------------------------------------------
      ; Accelerator CNTL-C was pressed to set digitize mode to Off.
      ;------------------------------------------------------------------
   
      IF (char EQ 'C') THEN Op_DisableEditObjects, state, retval
      
      ;------------------------------------------------------------------
      ; Accelerator CNTL-D was pressed to set digitize mode to Digitize.
      ;------------------------------------------------------------------
   
      IF (char EQ 'D') THEN Op_EnableDrawObjects, state, retval
      
      ;------------------------------------------------------------------
      ; Accelerator CNTL-F was pressed to load fire pixels.
      ;------------------------------------------------------------------
   
      IF (char EQ 'F') THEN Op_ReadFirePixels, state, retval
      
      ;------------------------------------------------------------------
      ; Accelerator CNTL-O was pressed to edit object display options.
      ;------------------------------------------------------------------
   
      IF (char EQ 'O') THEN Op_PlumeRegionOptions_gui, state, cancel
          
      ;------------------------------------------------------------------
      ; Accelerator CNTL-P was pressed to change color palette.
      ;------------------------------------------------------------------

      IF (char EQ 'P') THEN Op_SetColorPalette, state
         
      ;------------------------------------------------------------------
      ; Accelerator CNTL-R was pressed to set digitize mode to Delete.
      ;------------------------------------------------------------------
      
      IF (char EQ 'R') THEN Op_DeleteDrawObjects, state, retval
      
      ;------------------------------------------------------------------
      ; Accelerator CNTL-S was pressed to edit swath display options.
      ;------------------------------------------------------------------
   
      IF (char EQ 'S') THEN Op_MISRSwathOptions_gui, state
      
      ;------------------------------------------------------------------
      ; Accelerator CNTL-W was pressed to perform misregistration.
      ;------------------------------------------------------------------
   
      IF (char EQ 'W') THEN BEGIN
         Op_FixCoregErrorMulti, state, 3, 'N', $
                [0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0], state.curframe, $
                CoordStruct.(state.curframe GT 9).num_band, retval
      ENDIF
      
   ENDIF

   ;---------------------------------------------------------------------
   ; Left mouse button is being pressed.
   ;---------------------------------------------------------------------

   IF (event.type EQ 0 AND event.press EQ 1) THEN $
      state.mousebtnstatus = 2

   ;---------------------------------------------------------------------
   ; Right mouse button is being pressed.
   ;---------------------------------------------------------------------

   IF (event.type EQ 0 AND event.press EQ 4) THEN $
      state.mousebtnstatus = 1

   ;---------------------------------------------------------------------
   ; Center or right mouse button is being pressed.
   ;---------------------------------------------------------------------

   IF (event.type EQ 0 AND event.press EQ 2) THEN $
      state.mousebtnstatus = 3

   ;---------------------------------------------------------------------
   ; Mouse button is being released.
   ;---------------------------------------------------------------------

   IF (event.type EQ 1) THEN state.mousebtnstatus = 0

   ;---------------------------------------------------------------------
   ; Keyboard arrow keys are being invoked to move the cursor by 1 pixel
   ; increments.
   ;---------------------------------------------------------------------

   wait_for_operation_to_finish = 0
   
   IF ((event.type EQ 6) AND (event.press EQ 1) AND $
       (event.key GE 5)  AND (event.key LE 8)) THEN BEGIN
      wait_for_operation_to_finish = 1
      event.x = event.x - (event.key EQ 5)
      event.x = event.x + (event.key EQ 6)
      event.y = event.y + (event.key EQ 7)
      event.y = event.y - (event.key EQ 8)
      SWIN = !D.window
      SafeWSET, event.id, didit
      SafeWSET, state.draw_win, didit
      TVCRS,event.x,event.y
      SafeWSET, SWIN, didit

      state.mousebtnstatus = 2
   ENDIF

   ;---------------------------------------------------------------------
   ; Any mouse button is being held down. Update the coordinates.
   ;---------------------------------------------------------------------

   IF (N_ELEMENTS(state) NE 0) THEN BEGIN
      IF (state.mousebtnstatus GE 1 AND $
          event.X GE 0 AND event.Y GE 0 AND $
          event.X LT state.sizex AND $
          event.Y LT state.sizey) THEN BEGIN

         ShowCoordinates, state, event.X, event.Y
      ENDIF
   ENDIF

   ;---------------------------------------------------------------------
   ; Left mouse button is being pressed. Update the zoom window / BRF
   ; plot / draw-line operation / etc.
   ;---------------------------------------------------------------------

   IF (N_ELEMENTS(state) NE 0) THEN BEGIN
       IF (state.mousebtnstatus EQ 2 AND $
          event.X GE 0 AND event.Y GE 0 AND $
          event.X LT state.sizex AND $
          event.Y LT state.sizey) THEN BEGIN

         !VAR.BRFparms.BRF_INIT_PLOT = 0

         IF (!SAV.Digitize.DIG_STATE[0] GT 0) THEN $
            AddPointToObject, state, event
         IF (!SAV.Digitize.DIG_STATE[0] LT 0) THEN $
            DeleteObjOrPt, state, event
         IF (state.showBRFwndw) THEN $
            DrawZoomWindow, state, event
         IF (!VAR.AerRsrch.AerosolClassify EQ 1) THEN $
            BegGetAerosolBox, state, event
      ENDIF
   ENDIF

   ;---------------------------------------------------------------------
   ; Left mouse button is being released. Update the appropriate process.
   ;---------------------------------------------------------------------

   IF (N_ELEMENTS(state) NE 0) THEN BEGIN
       IF (state.mousebtnstatus EQ 0 AND $
          event.X GE 0 AND event.Y GE 0 AND $
          event.X LT state.sizex AND $
          event.Y LT state.sizey) THEN BEGIN

         IF (!VAR.AerRsrch.AerosolClassify EQ 1) THEN $
            EndGetAerosolBox, state, event
      ENDIF
   ENDIF

   ;---------------------------------------------------------------------
   ; Right mouse button is being pressed. Terminate the drawing or
   ; deleting of the current line.
   ;---------------------------------------------------------------------

   IF (N_ELEMENTS(state) NE 0) THEN BEGIN
       IF (state.mousebtnstatus EQ 1 AND $
          event.X GE 0 AND event.Y GE 0 AND $
          event.X LT state.sizex AND event.Y LT state.sizey) THEN BEGIN

         IF (!SAV.Digitize.DIG_STATE[0] NE 0) THEN BEGIN
            DoneDrawingObject, state, event
         ENDIF
      ENDIF
   ENDIF

   IF (wait_for_operation_to_finish EQ 1) THEN BEGIN
      WIDGET_CONTROL,event.id,SENSITIVE=1
      wait_for_operation_to_finish = 0
      state.mousebtnstatus = 0
   ENDIF

ENDIF
;------------------------------------------------------------------------
; Keyboard and mouse events end here.
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; Branch to control activated by user
;------------------------------------------------------------------------

CASE event.id OF

   wAnimateBase: $      
      
   ;---------------------------------------------------------------------
   ; Animation speed controls
   ;---------------------------------------------------------------------
 
    IF (N_ELEMENTS(state) NE 0) THEN BEGIN
      IF (state.framedelta NE 0) THEN BEGIN         ; Animation
         WIDGET_CONTROL, wAnimateBase, TIMER=state.delay

         nframes = NCAMS
         pwin = Temporary(*state.pwinHdl)

         curframe = state.curframe + state.framedelta    ; New frame

         Rate = 0.0
         IF (state.cycle) THEN BEGIN
            IF (curframe LT state.begframe) THEN BEGIN
               state.framedelta = 1
               curframe = state.begframe
               ttm = systime(1)
               tdiff = ttm - state.loop_start_t
               IF (tdiff GT 0) THEN Rate = nframes / tdiff
            ENDIF
            IF (curframe GT state.endframe) THEN BEGIN
               state.framedelta = -1
               curframe = state.endframe
            ENDIF
         ENDIF ELSE BEGIN
            WHILE (curframe LT state.begframe) DO $
               curframe = curframe + (state.endframe - $
                                      state.begframe + 1) ; In range
            WHILE (curframe GT state.endframe) DO $
               curframe = curframe - (state.endframe - $
                                      state.begframe + 1)
            IF (curframe EQ state.begframe) THEN BEGIN    ; Display rate?
               ttm = systime(1)
               tdiff = ttm - state.loop_start_t
               IF (tdiff GT 0) THEN $
                  Rate = (state.endframe - state.begframe + 1) / tdiff  ; Frames/Sec
            ENDIF
         ENDELSE

         IF (Rate NE 0.0) THEN BEGIN            ; Update frame rate
            str_Rate = STRING(CEIL(Rate), FORMAT='(I2)')
            WIDGET_CONTROL, state.wFramesPerSecLabel, SET_VALUE = $
                            'Frames/Sec: ' + str_Rate
            state.loop_start_t = ttm
         ENDIF

         *state.pwinHdl = Temporary(pwin)
         SWIN = !D.window
         ResetFrame, State, curframe, 1
         EMPTY
         SafeWSET, SWIN, didit

         IF (state.showBRFwndw) THEN BEGIN
            SafeWSET, ZOOM_WNDW, didit
            IF (didit GT 0) THEN $
               DrawZoomWindow, state, *state.zoom_event_ptr
         ENDIF

      ENDIF
    ENDIF
 
   ;---------------------------------------------------------------------
   ; Camera selection controls
   ;---------------------------------------------------------------------

   state.wAnimateButton1 : BEGIN
      IF (event.select EQ 1 AND state.animatebtn NE 1) THEN BEGIN
         state.curframe -= 9
         state.begframe = 1
         state.endframe = 9
         state.animatebtn = 1
         WIDGET_CONTROL, state.wFramesWhichSlider, $
                         SET_SLIDER_MIN=state.begframe, $
                         SET_SLIDER_MAX=state.endframe, $
                         SET_VALUE=state.curframe
         IF (state.framedelta EQ 0) THEN ResetFrame, state, 5, 1
      ENDIF
   END

   state.wAnimateButton2 : BEGIN
      IF (event.select EQ 1 AND state.animatebtn NE 2) THEN BEGIN
         state.curframe += 9
         state.begframe = 10
         state.endframe = 18
         state.animatebtn = 2
         WIDGET_CONTROL, state.wFramesWhichSlider, $
                         SET_SLIDER_MIN=state.begframe, $
                         SET_SLIDER_MAX=state.endframe, $
                         SET_VALUE=state.curframe
         IF (state.framedelta EQ 0) THEN ResetFrame, state, 14, 1
      ENDIF
   END

   state.wAnimateButton3 : BEGIN
      IF (event.select EQ 1 AND state.animatebtn NE 3) THEN BEGIN
         diff = NCAMS * (3 - state.animatebtn)
         state.curframe += diff
         state.begframe = 1
         state.endframe = 18
         state.animatebtn = 3
         WIDGET_CONTROL, state.wFramesWhichSlider, $
                         SET_SLIDER_MIN=state.begframe, $
                         SET_SLIDER_MAX=state.endframe, $
                         SET_VALUE=state.curframe
         IF (state.framedelta EQ 0) THEN ResetFrame, state, 5, 1
      ENDIF
   END

   ;---------------------------------------------------------------------
   ; BRF controls
   ;---------------------------------------------------------------------

   state.wFramesBrfButton : BEGIN
      WIDGET_CONTROL, state.wFramesBrfButton, GET_VALUE = temp
      IF (event.select EQ 1) THEN BEGIN
         state.showBRFwndw = 1
         !SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO
         WIDGET_CONTROL, state.wBrfParamsButton, SENSITIVE=1
         Op_DisableEditObjects, state, retval
      ENDIF ELSE BEGIN
         state.showBRFwndw = 0
         WIDGET_CONTROL, state.wBrfParamsButton, SENSITIVE=0
         SafeWDELETE, ZOOM_WNDW, didit
         ZOOM_WNDW = -1
      ENDELSE
   END
   
   state.wBrfParamsButton : BEGIN
      ; Do it twice just in case the BRF window is not on-screen.
      ; Because the Save button will be greyed out until the first
      ; time the BRF plot is requested, it is not possible to attempt
      ; to save when the BRF plot has not been initialized.  Thus,
      ; this save should always be successful.

      WIDGET_CONTROL, /HOURGLASS
      IF (~ PTR_VALID(state.zoom_event_ptr)) THEN BEGIN
         SetBrfplotValues, 1, state
      ENDIF ELSE BEGIN
         SetBrfplotValues, 0, state
         CASE !VAR.BRFparms.DO_BRF_PLOT OF
            0 : DrawZoomWindow, state, *state.zoom_event_ptr
            1 : DrawZoomWindow, state, *state.zoom_event_ptr, POSTSCRIPT=1, $
                                PS_IN_COLOR = 1
            2 : DrawZoomWindow, state, *state.zoom_event_ptr, POSTSCRIPT=1, $
                                PS_IN_COLOR = 0
            3 : DrawZoomWindow, state, *state.zoom_event_ptr, JPEG=1
            4 : DrawZoomWindow, state, *state.zoom_event_ptr, TIFF=1
            5 : DrawZoomWindow, state, *state.zoom_event_ptr, MP4=1
            ELSE:
         ENDCASE
      ENDELSE
   END

   state.wFramesSpeedSlider : BEGIN            ; New rate
      WIDGET_CONTROL, state.wFramesSpeedSlider, GET_VALUE = temp
      IF (temp EQ 100) THEN BEGIN
         state.delay = 0.
      ENDIF ELSE BEGIN
         state.delay = 4.0 / (1.0 + temp)
      ENDELSE
   END

   ;---------------------------------------------------------------------
   ; Color controls.
   ;---------------------------------------------------------------------

   state.wScalingMethodBtn : BEGIN
      !VAR.ClrScaleVals.ScalingMethod = (event.select EQ 1) ? 0 : 2
      IF (CoordStruct.(0).num_band EQ 1) THEN !VAR.ClrScaleVals.BandDisplayOpt = 4
      IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 1) THEN use_bands = [0,1,2,3]
      IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 2) THEN use_bands = [2]
      IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 3) THEN use_bands = [1]
      IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 4) THEN use_bands = [0]
      IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 5) THEN use_bands = [3]
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, use_bands
   END

   state.wBandsRGB : BEGIN
      !VAR.ClrScaleVals.BandDisplayOpt = 1
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, [0,1,2,3]
   END

   state.wBandsBlu : BEGIN
      !VAR.ClrScaleVals.BandDisplayOpt = 2
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, [2]
   END

   state.wBandsGrn : BEGIN
      !VAR.ClrScaleVals.BandDisplayOpt = 3
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, [1]
   END

   state.wBandsRed : BEGIN
      !VAR.ClrScaleVals.BandDisplayOpt = 4
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, [0]
   END

   state.wBandsNIR : BEGIN
      !VAR.ClrScaleVals.BandDisplayOpt = 5
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, state, [3]
   END

   state.wBrightCntlSlider : BEGIN       ; Brightness slider
      WIDGET_CONTROL, state.wBrightCntlSlider, GET_VALUE=new_val
      prev_val = FIX(!VAR.ClrScaleVals.Brightness * 100)

      IF (prev_val NE new_val) THEN BEGIN
         !VAR.ClrScaleVals.Brightness = new_val * 0.01
         post_str = STRING(FORMAT='(F4.2)', !VAR.ClrScaleVals.Brightness)
         WIDGET_CONTROL, state.wBrightnessValue, SET_VALUE=post_str
         whichorbit = 0
         WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state

         FOR cam = 1,(!KON.Instr.NCAM * !VAR.NumOrbits) DO BEGIN
           WIDGET_CONTROL, /HOURGLASS
           IF (CoordStruct.(0).num_band EQ 1) THEN BEGIN
              use_bands = [0]
           ENDIF ELSE BEGIN
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 1) THEN use_bands = $
                                                         !KON.Instr.RGBbands
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 2) THEN use_bands = [2]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 3) THEN use_bands = [1]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 4) THEN use_bands = [0]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 5) THEN use_bands = [3]
           ENDELSE

           PrepImageArray, 0, use_bands, state.nframes-1, cam-1, image, retval
           CW_ANIMATE_LOAD, state.wAnimateBase, FRAME=cam, IMAGE=image, /ORDER
           image = 0
         ENDFOR

         RedrawWindow, state, state.curframe
      ENDIF
   END

   state.wContrastCntlSlider : BEGIN  ; Contrast slider.
      WIDGET_CONTROL, state.wContrastCntlSlider, GET_VALUE=new_val
      prev_val = FIX(!VAR.ClrScaleVals.Contrast * 100)
      
      IF (prev_val NE new_val) THEN BEGIN
         !VAR.ClrScaleVals.Contrast = new_val * 0.01
         post_str = STRING(FORMAT='(F4.2)', !VAR.ClrScaleVals.Contrast)
         WIDGET_CONTROL, state.wContrastValue, SET_VALUE=post_str
         whichorbit = 0
         WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
             
         FOR cam = 1,(!KON.Instr.NCAM * !VAR.NumOrbits) DO BEGIN
            WIDGET_CONTROL, /HOURGLASS
           IF (CoordStruct.(0).num_band EQ 1) THEN BEGIN
              use_bands = [0]
           ENDIF ELSE BEGIN
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 1) THEN use_bands = $
                                                         !KON.Instr.RGBbands
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 2) THEN use_bands = [2]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 3) THEN use_bands = [1]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 4) THEN use_bands = [0]
              IF (!VAR.ClrScaleVals.BandDisplayOpt EQ 5) THEN use_bands = [3]
           ENDELSE
            
           PrepImageArray, 0, use_bands, state.nframes-1, cam-1, image, retval
           CW_ANIMATE_LOAD, state.wAnimateBase, FRAME=cam, IMAGE=image, /ORDER
           image = 0
         ENDFOR
         
         RedrawWindow, state, state.curframe
      ENDIF
   END
   
   state.wFramesWhichSlider : BEGIN
      WIDGET_CONTROL, state.wFramesWhichSlider, GET_VALUE=temp
      IF (temp NE state.curframe) THEN BEGIN
         RedrawWindow, state, temp
         WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = 'Camera: ' + $
                         !KON.Instr.camera_names[temp]
         !VAR.DataSwath.CURRENT_WNDW = state.curframe
         IF (ZOOM_WNDW GE 0) THEN BEGIN
            DEVICE, WINDOW_STATE=window_state
            IF (ZOOM_WNDW GE (SIZE(window_state))[1]) THEN BEGIN
               ZOOM_WNDW = -1
            ENDIF ELSE BEGIN
               IF (window_state[ZOOM_WNDW] EQ 1 AND state.showBRFwndw) THEN $
                  DrawZoomWindow, state, *state.zoom_event_ptr
            ENDELSE
            window_state = 0
         ENDIF
      ENDIF
   END

   ;---------------------------------------------------------------------
   ; Buttons for toggling data overlays.
   ;---------------------------------------------------------------------

   state.wFramesDotsButton : BEGIN   ;   reference marks
      state.showdots = (event.select EQ 1) ? 1 : 0
      RedrawWindow, state, state.curframe
   END

   state.wFramesCountryBtn : BEGIN   ;   country boundaries from IDL
      state.showcountry = (event.select EQ 1) ? 1 : 0
      RedrawWindow, state, state.curframe
   END

   state.wFramesLLgridBtn : BEGIN   ;   lat/lon grid from IDL
      state.showLLgrid = (event.select EQ 1) ? 1 : 0
      RedrawWindow, state, state.curframe
   END
   
   state.wFramesHolesButton : BEGIN   ;   holes in (obscured) terrain
      !VAR.CurrFiles.TerrHoles_Loaded = (event.select EQ 1) ? 1 : 0
      use_bands = (CoordStruct.(0).num_band EQ 1) ? [0] : [0,1,2,3]
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE=state
      RescaleColorsForDiffBands, State, use_bands
   END
   
   state.wFramesLinesButton : BEGIN   ;   drawn lines
      state.showobjects = (event.select EQ 1) ? 1 : 0
      RedrawWindow, state, state.curframe
   END
   
   state.wFramesPixColorBtn : BEGIN   ;   data value colors
      state.showmapclr = (event.select EQ 1) ? 1 : 0
      RedrawWindow, state, state.curframe
   END
  
   state.wFramesFireButton : BEGIN   ;  fire points read from file
      state.showfire = (event.select EQ 1) ? 1 : 0
      IF (~ !VAR.CurrFiles.Fire_Loaded) THEN state.showfire = 0
      RedrawWindow, state, state.curframe
      IF (state.showfire EQ 1) THEN RedrawModisFiresInOps, State
   END
   
   state.wFramesMarkerButton : BEGIN   ;   marker points read from file
      state.showmarker = (event.select EQ 1) ? 1 : 0
      IF (~ !VAR.CurrFiles.Marker_Loaded) THEN state.showmarker = 0
      RedrawWindow, state, state.curframe
   END
  
   ;---------------------------------------------------------------------
   ; Load PDF Help file.
   ;---------------------------------------------------------------------

   state.wHelpButton : BEGIN
      ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + 'MINXdoc_AnimationWindow.pdf'
   END
 
   ;---------------------------------------------------------------------
   ; Selecting option from task menu.
   ;---------------------------------------------------------------------

   state.wFramesMenuList : BEGIN
      menu_pick = 0

      IF (event.value EQ 'PDF Help ...' OR $
          event.value EQ 'Save Session ...' OR $
          event.value EQ 'Restore Session ...' OR $
          event.value EQ 'Save this Camera image for Google Earth ...' OR $
          event.value EQ 'Save this Camera image as GeoTIFF ...' OR $
          event.value EQ 'Save this Camera image as TIFF ...' OR $
          event.value EQ 'Save this Camera image as JPEG ...' OR $
          event.value EQ 'Save this Camera image as PNG ...' OR $
          event.value EQ 'Save this Camera image as GIF ...' OR $
          event.value EQ 'Save Camera images in 9 TIFF files ...' OR $
          event.value EQ 'Save Camera images in 9 JPEG files ...' OR $
          event.value EQ 'Save 9 Camera images in 1 GIF file ...' OR $
          event.value EQ 'Save 9 Camera images in 1 MP4 file ...' OR $
          event.value EQ 'Write MISR Data to Text File ...' OR $
          event.value EQ 'Set Dig. Display Options ... CNTL-O' OR $
          event.value EQ 'Set Swath Display Options ... CNTL-S' OR $
          event.value EQ 'Select Color Palette ... CNTL-P' OR $
          event.value EQ 'Post Marker Pixels from File ...') THEN $
        menu_pick = 1

      IF (event.value EQ 'Warp Orbit 1 Cameras for Plumes CNTL-W' OR $
          event.value EQ 'Warp Orbit 2 An to Match Orbit 1 An ...' OR $
          event.value EQ 'Warp Orbit 1 and 2 Cameras to Match An' OR $
          event.value EQ 'Warp Current Camera to Match An ...' OR $
          event.value EQ 'Enable Digitizing ... CNTL-D' OR $
          event.value EQ 'Enable Deleting CNTL-R' OR $
          event.value EQ 'Enable Redigitizing ...' OR $
          event.value EQ 'Disable all Options CNTL-C' OR $
          event.value EQ 'Aerosol Classifier' OR $
          event.value EQ 'Load Modis Fire Pixels ... CNTL-F') THEN BEGIN
        menu_pick = 2

      ENDIF

      IF (state.curframe LE 0 AND menu_pick EQ 0) THEN BEGIN
            state.curframe = 5
            WIDGET_CONTROL, state.wFramesWhichSlider, $
                            SET_VALUE = state.curframe
            WIDGET_CONTROL, state.wFramesByCamLabel, $
                            SET_VALUE = 'Camera: ' + $
                            !KON.Instr.camera_names[state.curframe]
      ENDIF

      IF (state.curframe GT 0 OR menu_pick EQ 1) THEN BEGIN

         curframe = state.curframe
         whichorbit = (curframe GT 9)
         numbands = CoordStruct.(whichorbit).num_band
         rtrn_val = 0

         pwin_work = (*state.pwinHdl)[!VAR.WORK_WNDW]
         
         IF (menu_pick EQ 0) THEN SafeWSET, pwin_work, didit
         (*state.pwinHdl)[!VAR.WORK_WNDW] = pwin_work
         
         ; Don't do this for any of the Save functions.
         IF (menu_pick EQ 2) THEN state.curframe = !VAR.WORK_WNDW

         IF (menu_pick NE 1) THEN BEGIN
            WIDGET_CONTROL, state.wFramesWhichSlider, $
                            SET_VALUE = !VAR.WORK_WNDW
            WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = 'Camera: ' + $
                            !KON.Instr.camera_names[!VAR.WORK_WNDW]
         ENDIF
         WIDGET_CONTROL, /HOURGLASS

         CASE event.value OF
            'PDF Help ...'  : ONLINE_HELP, BOOK='data' + !KON.Misc.Slash + $
                                           'MINXdoc_TaskMenu.pdf'
            'Save Session ...'    : Op_SaveSession,    state, retval
            'Restore Session ...' : Op_RestoreSession, state, retval
            'Save this Camera image as TIFF ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_TIF, 1, '', 0
            'Save this Camera image as JPEG ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_JPG, 1, '', 0
            'Save this Camera image as GIF ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_GIF, 1, '', 0
            'Save this Camera image as PNG ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_PNG, 1, '', 0
            'Save this Camera image for Google Earth ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_KML, 1, '', 0
            'Save this Camera image as GeoTIFF ...' : $
                  Op_WriteImageFile, state, curframe, $
                                     curframe, !KON.PlotTyp.DO_GTIF,1, '', 0
            'Save Camera images in 9 TIFF files ...' : $
                  Op_WriteImageFile, state, 1, 9, $
                                    !KON.PlotTyp.DO_TIF, 1, '', 0
            'Save Camera images in 9 JPEG files ...' : $
                  Op_WriteImageFile, state, 1, 9, $
                                    !KON.PlotTyp.DO_JPG, 1, '', 0
            'Save 9 Camera images in 1 GIF file ...' : $
                  Op_WriteImageFile, state, 1, 9, $
                                    !KON.PlotTyp.DO_MGIF,1, '', 0
            'Save 9 Camera images in 1 MP4 file ...' : $
                  Op_WriteImageFile, state, 1, 9, $
                                    !KON.PlotTyp.DO_MP4, 1, '', 0
            'Write MISR Data to Text File ...' : $
                  Op_WriteTextFile, state, curframe
            'Warp Orbit 1 Cameras for Plumes CNTL-W' : $
                  Op_FixCoregErrorMulti, state, 3, 'N', $
                             [0,1,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0], $
                                        curframe, numbands, retval
            'Warp Orbit 2 An to Match Orbit 1 An ...' : $
                  Op_FixCoregErrorAn, 0, state, 1, 'Y', $
                                      curframe, numbands, retval
            'Warp Orbit 1 and 2 Cameras to Match An' : $
                  Op_FixCoregErrorMulti, state, 1, 'N', $
                             [0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1], $
                                        curframe, numbands, retval
            'Warp Current Camera to Match An ...' : $
                  Op_FixCoregError, 0, 1, state, curframe, 'Y', 1, numbands, $
                                    nxy, meanx, meany, stddevx, stddevy, retval
            'Post Marker Pixels from File ...' : Op_ReadMarkerPixels, state, $
                                                 curframe, retval
            'Enable Digitizing ... CNTL-D' : Op_EnableDrawObjects, state, $
                                             retval
            'Enable Deleting CNTL-R': Op_DeleteDrawObjects, state, retval
            'Enable Redigitizing ...' : Op_EnableRedigObjects, state, retval
            'Disable all Options CNTL-C': Op_DisableEditObjects, state, retval
            'Load Modis Fire Pixels ... CNTL-F' : Op_ReadFirePixels, state, $
                                                  retval
            'Select Color Palette ... CNTL-P' : Op_SetColorPalette, state
            'Set Dig. Display Options ... CNTL-O' : Op_PlumeRegionOptions_gui, $
                                                        state, cancel
            'Set Swath Display Options ... CNTL-S' : Op_MISRSwathOptions_gui, $
                                                     state

            'Red/Blue 3D ...'     : Op_RedBlue3D, state, rtrn_val
            'MISR Vision RGB ...' : Op_MISRVision, state, rtrn_val
            'Df Orbit Diff'       : Op_OrbitDiff, state, 0, rtrn_val
            'Cf Orbit Diff'       : Op_OrbitDiff, state, 1, rtrn_val
            'Bf Orbit Diff'       : Op_OrbitDiff, state, 2, rtrn_val
            'Af Orbit Diff'       : Op_OrbitDiff, state, 3, rtrn_val
            'An Orbit Diff'       : Op_OrbitDiff, state, 4, rtrn_val
            'Aa Orbit Diff'       : Op_OrbitDiff, state, 5, rtrn_val
            'Ba Orbit Diff'       : Op_OrbitDiff, state, 6, rtrn_val
            'Ca Orbit Diff'       : Op_OrbitDiff, state, 7, rtrn_val
            'Da Orbit Diff'       : Op_OrbitDiff, state, 8, rtrn_val

            'ABS(Blue-Green)'     : Op_BandDiff, state, 4, 1, rtrn_val
            'ABS(Blue-Red)'       : Op_BandDiff, state, 4, 2, rtrn_val
            'ABS(Blue-NIR)'       : Op_BandDiff, state, 4, 3, rtrn_val
            'Df-Da Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 10, rtrn_val
            'Cf-Ca Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 11, rtrn_val
            'Bf-Ba Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 12, rtrn_val
            'Af-Aa Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 13, rtrn_val
            'Aa-Af Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 15, rtrn_val
            'Ba-Bf Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 16, rtrn_val
            'Ca-Cf Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 17, rtrn_val
            'Da-Df Orbit Diff - Red-NIR' : Op_OrbitDiff, state, 18, rtrn_val
            'Aerosol Classifier'  : Op_ClassifyAS, state, retval
            'Scaled RGB'          : Op_HiPassFilter, state, curframe, 1, retval
            'Blue/Red Ratio'      : Op_HiPassFilter, state, curframe, 2, retval

            'Roberts: DfAnDa'     : Op_EdgeEnhance, state, 0, 0, retval
            'Roberts: CfAnCa'     : Op_EdgeEnhance, state, 0, 1, retval
            'Sobel: DfAnDa'       : Op_EdgeEnhance, state, 1, 0, retval
            'Sobel: CfAnCa'       : Op_EdgeEnhance, state, 1, 1, retval
            'Mean, StdDev, Skewness' : Op_MeanStddevSkew, state
            'Mean Df->Bf,Mean Af->Aa,Mean Ba->Da' : Op_MeanMeanMean,state
            'Quadratic Fit: A, B, C' : Op_QuadraticFit, state
            'Df/An'               : Op_CamDivNadir, state, 0, retval
            'Cf/An'               : Op_CamDivNadir, state, 1, retval
            'Bf/An'               : Op_CamDivNadir, state, 2, retval
            'Af/An'               : Op_CamDivNadir, state, 3, retval
            'Aa/An'               : Op_CamDivNadir, state, 5, retval
            'Ba/An'               : Op_CamDivNadir, state, 6, retval
            'Ca/An'               : Op_CamDivNadir, state, 7, retval
            'Da/An'               : Op_CamDivNadir, state, 8, retval
            'Df/Da'               : Op_CamRatio_2, state, 0, retval
            'Cf/Ca'               : Op_CamRatio_2, state, 1, retval
            'Bf/Ba'               : Op_CamRatio_2, state, 2, retval
            'Af/Aa'               : Op_CamRatio_2, state, 3, retval
            'Df/Da, Cf/Ca, Bf/Ba' : Op_ForeVsAft, state, 0, retval
            'Cf/Ca, Bf/Ba, Af/Aa' : Op_ForeVsAft, state, 1, retval
            'Df-Da'               : Op_CamDiff, state, 0, retval
            'Cf-Ca'               : Op_CamDiff, state, 1, retval
            'Bf-Ba'               : Op_CamDiff, state, 2, retval
            'Af-Aa'               : Op_CamDiff, state, 3, retval
            '(Df-Da)/(Df+Da)'     : Op_CamRatio_1, state, 0, retval
            '(Cf-Ca)/(Cf+Ca)'     : Op_CamRatio_1, state, 1, retval
            '(Bf-Ba)/(Bf+Ba)'     : Op_CamRatio_1, state, 2, retval
            '(Af-Aa)/(Af+Aa)'     : Op_CamRatio_1, state, 3, retval
            'Df-Cf, Cf-Bf, Bf-Af' : Op_ForeDiff, state, 0, retval
            'Cf-Bf, Bf-Af, Af-An' : Op_ForeDiff, state, 1, retval
            'Ca-Ba, Ba-Aa, Aa-An' : Op_AftDiff, state, 1, retval
            'Da-Ca, Ca-Ba, Ba-Aa' : Op_AftDiff, state, 0, retval
            'Dx-Cx, Cx-Bx, Bx-Ax' : Op_ForeVsAftDiff, state, 0, retval
            'Cx-Bx, Bx-Ax, Ax-An' : Op_ForeVsAftDiff, state, 1, retval
            'Cx+Bx, Bx+Ax, Ax+An' : Op_ForeVsAftSum, state, 1, retval
            'Dx+Cx, Cx+Bx, Bx+Ax' : Op_ForeVsAftSum, state, 0, retval
         ELSE: BEGIN
            mssg = 'Error - no valid operation was selected.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         END
         ENDCASE

         IF (menu_pick EQ 0 AND rtrn_val EQ 0) THEN BEGIN
            RedrawWindow, state, !VAR.WORK_WNDW
         ENDIF ELSE BEGIN
            IF (event.value NE 'Restore Session ...') THEN BEGIN
               WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = curframe
               WIDGET_CONTROL, state.wFramesByCamLabel, SET_VALUE = 'Camera: ' + $
                               !KON.Instr.camera_names[curframe]
            ENDIF
         ENDELSE

      ENDIF ELSE BEGIN
         mssg = 'You must first select a valid camera to process.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDELSE

   END

   ;---------------------------------------------------------------------
   ; Controls for displaying MISR swath products
   ;---------------------------------------------------------------------

   state.wFramesProductMenu : BEGIN  ;   select MISR product to display
      CASE event.value OF
         !VAR.DataProd[ (ndx=1)].Field : data_type = !KON.DataProd.TYPE_TERRAIN_HT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LANDH2O_MASK
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_BIOME_MAP
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_SUN_ZENITH
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_SUN_AZIMUTH
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_DF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_CF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_BF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_AF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_AN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_AA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_BA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_CA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_ZEN_DA
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_DF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_CF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_BF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_AF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_AN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_AA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_BA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_CA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_AZI_DA
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_DF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_CF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_BF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_AF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_AN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_AA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_BA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_CA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_SCT_DA
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_DF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_CF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_BF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_AF
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_AN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_AA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_BA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_CA
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CAM_GLT_DA
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_BE_TAU_B
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_BE_TAU_G
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_BE_TAU_R
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_BE_TAU_N
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_LR_TAU_B
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_LR_TAU_G
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_LR_TAU_R
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_LR_TAU_N
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_SSA_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_SSA_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_SSA_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_SSA_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_BE_ANGXP
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_LR_ANGXP
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_FRC_SML
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_FRC_MED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_FRC_LRG
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_FRC_SPH
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_AER_FRC_NOS
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_BHR_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_BHR_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_BHR_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_BHR_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_DHR_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_DHR_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_DHR_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_DHR_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_NDVI
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_RPV1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_RPV2
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_LND_RPV3
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_STER_ZEROHT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_STER_CORRHT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_STER_WNDCRS
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_STER_WNDALG
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_STER_SDCM
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDZ_ZEROHT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDZ_WNDCRS
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDZ_CLDMSK
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDC_CORRHT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDC_WNDCRS
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLDC_SDCM
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLD_MOTNHT
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLD_WNDCRS
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLD_WNDALG
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLD_CLDMSK
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLASS_SMOKE
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLASS_DUST
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLASS_CLOUD
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_CLASS_CLEAR
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_LOC_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_LOC_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_LOC_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_LOC_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_LOC_BRD
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_RES_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_RES_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_RES_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_RES_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_RES_BRD
         !VAR.DataProd[(ndx+=1)].Field : data_type = -1
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_EXP_BLU
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_EXP_GRN
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_EXP_RED
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_EXP_NIR
         !VAR.DataProd[(ndx+=1)].Field : data_type = !KON.DataProd.TYPE_ALB_EXP_BRD

         ELSE: BEGIN
            mssg = 'Error - no valid MISR product was selected.'
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            data_type = -1
         END
      ENDCASE

      IF (data_type GT 0) THEN BEGIN
         TestAndLoadProducts, state, CoordStruct, ProductList, event.value, $
                              data_type, current_value, rtrnval
         IF (rtrnval EQ 0) THEN BEGIN
            WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE=1
            WIDGET_CONTROL, state.wFramesShowProdBtn, SET_VALUE=current_value
         ENDIF
      ENDIF
   END

   state.wFramesShowProdBtn : BEGIN  ;   show MISR product selection
      IF (event.select EQ 1) THEN BEGIN  ;  left button down
         !VAR.DataSwath.CURRENT_WNDW = state.curframe
         ShowDataProducts, state
      ENDIF
      IF (event.select EQ 0) THEN $
         ResetFrame, state, !VAR.DataSwath.CURRENT_WNDW, 1 ;  left button up
   END

   ;---------------------------------------------------------------------
   ; Animation controls
   ;---------------------------------------------------------------------
   
   state.wCyclePlayButton : BEGIN
      StartAnimation, state, wAnimateBase, CoordStruct.(0).num_band, 1, 1
   END

   state.wPlayButton : BEGIN
      StartAnimation, state, wAnimateBase, CoordStruct.(0).num_band, 1, 0
   END
   
   state.wReversePlayButton : BEGIN
      StartAnimation, state, wAnimateBase, CoordStruct.(0).num_band, -1, 0
   END
   
   state.wPauseButton : BEGIN
      state.framedelta = 0
      SetMyBitmapButtons, state
  
      IF (CoordStruct.(0).num_band GE 3) THEN BEGIN
         WIDGET_CONTROL, state.wBandsRGB, SENSITIVE = 1
         WIDGET_CONTROL, state.wBandsBlu, SENSITIVE = 1
         WIDGET_CONTROL, state.wBandsGrn, SENSITIVE = 1
         WIDGET_CONTROL, state.wBandsRed, SENSITIVE = 1
         WIDGET_CONTROL, state.wBandsNIR, SENSITIVE = 1
      ENDIF

      frame_psec = 'Frames/Sec: ' + ((!KON.Misc.MINX_PLATFORM EQ 1) ? $
                   ' 0' : ' 0 ')
      WIDGET_CONTROL, state.wScalingMethodBtn,  SENSITIVE = 1
      WIDGET_CONTROL, state.wBrightCntlSlider,  SENSITIVE = 1
      WIDGET_CONTROL, state.wContrastCntlSlider,SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesPerSecLabel, SET_VALUE = frame_psec
            
      n_orbits = 1 + ((state.nframes - 1) EQ 18)
      state.begframe = 0
      state.endframe = 9 * n_orbits
                        
      IF (n_orbits EQ 2) THEN BEGIN
         state.animatebtn = 3
         WIDGET_CONTROL, state.wAnimateButton3, /SET_BUTTON
      ENDIF
           
      WIDGET_CONTROL, state.wFramesWhichSlider, $
                      SET_SLIDER_MIN=state.begframe, $
                      SET_SLIDER_MAX=state.endframe, $
                      SET_VALUE=state.curframe, SENSITIVE = 1
                      
      is_terr = (!VAR.L1B2_TYPE EQ 'TERRAIN')
      WIDGET_CONTROL, state.wFramesHolesButton, SENSITIVE=is_terr
   END

   ;---------------------------------------------------------------------
   ; This is not used.
   ;---------------------------------------------------------------------

   state.wOpenButton : BEGIN

      ; get a copy before the structure is killed
      open_func           = state.open_func
      framedelta          = state.framedelta
      wFramesSpeedSlider  = state.wFramesSpeedSlider
      wScalingMethodBtn   = state.wScalingMethodBtn
      wBrightCntlSlider   = state.wBrightCntlSlider
      wContrastCntlSlider = state.wContrastCntlSlider

      ; Need to restore state since the following routines use it
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY
      CW_ANIMATE_RUN, wAnimateBase, /STOP

      ; Disable all controls until all frames are loaded
      WIDGET_CONTROL, wAnimateBase, SENSITIVE = 0
      fileOK = CALL_FUNCTION(open_func, event.top, wAnimateBase)

      IF fileOK THEN BEGIN
         WIDGET_CONTROL, wFramesSpeedSlider, GET_VALUE = rate
         IF framedelta EQ 0 THEN $
            framedelta = 1
         CW_ANIMATE_RUN, wAnimateBase, rate, DELTA=framedelta, $
                         /LASTFRAME
      ENDIF ELSE $
         ; Disable all controls until all frames are loaded
         WIDGET_CONTROL, wAnimateBase, SENSITIVE = 1

      ; Need structure back - This kills the old uvalue
      WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY
   END
   
   ;------------------------------------------------------------------------
   ; Exit out of the current orbit.
   ;------------------------------------------------------------------------

   state.wEndAnimationButton: BEGIN

      ;---------------------------------------------------------------------
      ; Destroy any visible region or swath dialog or color key windows.
      ;---------------------------------------------------------------------
      
      DestroyColorKey, 1  ; destroy region windows
      ResetSwathData
   
      !SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO
      SafeWSET, state.draw_win, didit
      SetMINXCursor, 0, rtrnval
      SafeWDELETE, ZOOM_WNDW, didit
      ZOOM_WNDW = -1

      ret = {WIDGET_KILL_REQUEST, ID:wAnimateBase, TOP:event.top, HANDLER:0L}
      state.nokill_pixmaps = 0  ;  added to release animation memory
   END
   ELSE:
  
ENDCASE

;------------------------------------------------------------------------
; If restoring session, don't call widget_control because wTopWorkBase
; won't exist.
;------------------------------------------------------------------------

IF (WIDGET_INFO(wTopWorkBase, /VALID_ID)) THEN $
   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY ; Restore state

RETURN, ret

END  ;  CW_ANIMATE_EVNT

;***************************************************************************
PRO StartAnimation, State, wAnimateBase, Numband, Framedelta, Cycle
;***************************************************************************
; Do this whenever user presses one of the 3 buttons that start animation.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

WIDGET_CONTROL, state.wBrightCntlSlider,   SENSITIVE = 0
WIDGET_CONTROL, state.wContrastCntlSlider, SENSITIVE = 0

IF (Numband GE 3) THEN BEGIN
   WIDGET_CONTROL, state.wBandsRGB, SENSITIVE = 0
   WIDGET_CONTROL, state.wBandsBlu, SENSITIVE = 0
   WIDGET_CONTROL, state.wBandsGrn, SENSITIVE = 0
   WIDGET_CONTROL, state.wBandsRed, SENSITIVE = 0
   WIDGET_CONTROL, state.wBandsNIR, SENSITIVE = 0
ENDIF ELSE BEGIN
   WIDGET_CONTROL, state.wBandsRed, SENSITIVE = 0
ENDELSE

WIDGET_CONTROL, state.wScalingMethodBtn,  SENSITIVE = 0
WIDGET_CONTROL, State.wFramesWhichSlider, SENSITIVE = 0
WIDGET_CONTROL, State.wFramesSpeedSlider, SENSITIVE = 1
IF (State.framedelta EQ 0) THEN $
   WIDGET_CONTROL, wAnimateBase, TIMER=State.delay
   
State.framedelta = Framedelta
State.cycle = Cycle
SetMyBitmapButtons, State

CASE 1 OF
   State.animatebtn EQ 1 : BEGIN
      State.begframe = 1
      State.endframe = 9
   END
   State.animatebtn EQ 2 : BEGIN
      State.begframe = 10
      State.endframe = 18
   END
   State.animatebtn EQ 3 : BEGIN
      State.begframe = 1
      State.endframe = 18
   END
ENDCASE

END  ;  StartAnimation

;***************************************************************************
PRO CW_ANIMATE_LOAD, wAnimateBase, IMAGE=image, FRAME=frame, ORDER=order, $
                     WINDOW=window, XOFFSET=xoffset, YOFFSET=yoffset, $
                     CYCLE=cycle
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY
pwin = Temporary(*state.pwinHdl)

old_window = !D.WINDOW

displayload = WIDGET_INFO(wAnimateBase, /REALIZED)
IF (displayload NE 0) THEN BEGIN
   WIDGET_CONTROL, state.wDrawWindow, GET_VALUE=temp
   state.draw_win = temp
   SafeWSET, state.draw_win, didit
   ; In case the draw widget size is different than that requested.
   state.sizex = !D.X_VSIZE
   state.sizey = !D.Y_VSIZE
ENDIF

; Default values and range checking
IF (N_ELEMENTS(yoffset) EQ 0) THEN $
   yoffset = 0
IF (N_ELEMENTS(xoffset) EQ 0) THEN $
   xoffset = 0
IF (N_ELEMENTS(frame)) GT 0 THEN BEGIN
   IF (frame LT 0) OR (frame GE N_ELEMENTS(pwin)) THEN $
      MESSAGE, "Frame number must be from 0 to nframes -1."
ENDIF ELSE $
   frame=0

jj = N_ELEMENTS(window)            ; check to see if WINDOW was set

IF (jj GT 0) THEN BEGIN            ; Copy image from window?
   IF (jj LT 5) THEN BEGIN         ; If coords not spec, use all
      SafeWSET, window[0], didit
      p = [window[0], 0, 0, !D.X_VSIZE, !D.Y_VSIZE]  ; Get window size
   ENDIF ELSE $
      p = window

   IF pwin[frame] LT 0 THEN BEGIN            ; Create pixwin?
      WINDOW, /FREE, XSIZE = state.sizex, YSIZE = state.sizey, /PIXMAP
      pwin[frame] = !D.WINDOW
   ENDIF

   IF (p[3] GT state.sizex) OR (p[4] GT state.sizey) THEN $
      MESSAGE, "Window parameter larger than setup"

   IF (displayload) THEN BEGIN                 ; Load display window
      SafeWSET, state.draw_win, didit          ; Show it?
      IF state.draw_win NE p[0] THEN $         ; Copy to show window?
         DEVICE, COPY = [p[1],p[2],p[3],p[4],xoffset,yoffset,p[0]]
   ENDIF ELSE BEGIN                            ; load / no show
      SafeWSET, pwin[frame], didit
      DEVICE, COPY = [p[1], p[2], p[3], p[4], xoffset, yoffset, p[0]]
   ENDELSE
   
   IF (state.showdots    EQ 1) THEN $
      RedrawRefDots, state, 0, 0, state.sizex, state.sizey
   IF (state.showcountry EQ 1) THEN RedrawGeography, state, 1
   IF (state.showLLgrid  EQ 1) THEN RedrawGeography, state, 2
   IF (state.showmapclr  EQ 1) THEN $
      RedrawMapColors, state, 0, 0, state.sizex, state.sizey, 0
   IF (state.showobjects EQ 1) THEN $
      RedrawObjects, state, 0, 0, state.sizex, state.sizey, 0
   IF (state.showobjects EQ 1) THEN $
      RedrawDirArrow, state, 0, 0, state.sizex, state.sizey, 0
   IF (state.showmarker  EQ 1) THEN $
      RedrawMarkerPts, state, 0, 0, state.sizex, state.sizey

   IF (displayload) THEN BEGIN
      SafeWSET, pwin[frame], didit       ;Pixmap destination
      DEVICE, COPY = [xoffset, yoffset, p[3], p[4], xoffset, yoffset, $
              state.draw_win]            ;Copy from display window to pixmap
   ENDIF

   EMPTY

   IF (N_ELEMENTS(state.draw_win) EQ 0) THEN state.draw_win = -1

   SafeWSET, old_window, didit

   ; When displayload is set, the frame slider should update to
   ; show frame number
   IF (displayload) THEN $
      WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = frame
   GOTO, Done
ENDIF                                    ;So WINDOW was not set.

IF N_ELEMENTS(image) NE 0 THEN BEGIN     ;Make sure image was set.

   ;Make sure the image is of a valid size and report if not.
   ndims = SIZE(image, /N_DIMENSIONS)
   IF ndims LT 2 OR ndims GT 3 THEN $
      MESSAGE, "Image parameter must have 2 or 3 dimensions."
   truecolor = ndims eq 3
   sz = SIZE(image, /DIMENSIONS)
   interleave = truecolor * ((WHERE(sz eq 3))[0] + 1)
   CASE interleave OF
      1 : XY = sz[1:2]
      2 : XY = sz[[0, 2]]
      ELSE : XY = sz[0:1]
   ENDCASE

   IF ((XY[0] NE state.sizex) AND (XY[1] NE state.sizey)) THEN $
      MESSAGE, "Image parameter must be of size " + $
                STRING(state.sizex)+ STRING(state.sizey)

   IF N_ELEMENTS(order) EQ 0 THEN $
      ORDER = 0        ;Default order
   IF pwin[frame] LT 0 THEN BEGIN
      WINDOW, /FREE, xsize = state.sizex, ysize = state.sizey, /PIXMAP
      pwin[frame] = !D.WINDOW
   ENDIF ELSE BEGIN
      SafeWSET, pwin[frame], didit
   ENDELSE

   TV, image, xoffset, yoffset, ORDER=order, TRUE=interleave
   EMPTY

   ; When displayload is set, the draw widget should be updated
   ; to show the new frame being loaded and the frame slider
   ; should be set correspondingly

   IF (displayload NE 0) THEN BEGIN
      WIDGET_CONTROL, state.wFramesWhichSlider, SET_VALUE = frame
      SafeWSET, state.draw_win, didit
      TV, image, xoffset, yoffset, ORDER=order, TRUE=interleave
      EMPTY
   ENDIF

   SafeWSET, old_window, didit
   GOTO, Done
ENDIF                                    ;End of "if IMAGE was set".

Done: *state.pwinHdl = Temporary(pwin)
; Restore uvalue
WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY

END ;  CW_ANIMATE_LOAD

;***************************************************************************
PRO CW_ANIMATE_RUN, wAnimateBase, rate, STOP = stop, NFRAMES = nframes, $
                    DELTA = delta, LASTFRAME=lastFrame
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

   wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
   WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY

   old_window = !D.WINDOW                        ;Save old window

   ; Refuse to run if the cluster isn't realized.
   IF (WIDGET_INFO(wAnimateBase, /REALIZED) EQ 0) THEN $
      MESSAGE,'Animation widget must be realized before it can run'

   IF KEYWORD_SET(stop) THEN BEGIN           ;Stop the animation
      state.framedelta = 0                ;This shows we've stopped.
      WIDGET_CONTROL, state.wFramesWhichSlider, $
                      SET_VALUE = state.curframe
      WIDGET_CONTROL, state.wFramesWhichSlider, SENSITIVE = 1
      SetMyBitmapButtons, state
      GOTO, done
   ENDIF

   ; It is realized now, so get the draw widget window ID.
   WIDGET_CONTROL, GET_VALUE=temp, state.wDrawWindow
   state.draw_win = temp
   SafeWSET, state.draw_win, didit
   EMPTY

   WIDGET_CONTROL, wAnimateBase, /SENSITIVE

   IF N_ELEMENTS(nframes) GT 0 THEN BEGIN        ;Nframes spec?
      pwin = Temporary(*state.pwinHdl)
      IF nframes GT N_ELEMENTS(pwin) THEN $
         MESSAGE, 'Run called with too many frames'
      *state.pwinHdl = Temporary(pwin)
      state.nframes = nframes
   ENDIF

   ;Set up the initial values used by the background task
   IF N_ELEMENTS(lastFrame) NE 0 THEN BEGIN
      state.curframe = state.nframes
      state.begframe = 1
      state.endframe = 9
   ENDIF ELSE BEGIN
       state.curframe = 0
       state.begframe = 1
       state.endframe = 9
   ENDELSE

   IF N_ELEMENTS(delta) NE 0 THEN $
      state.framedelta = -1 > delta < 1 $     ; In range?
   ELSE $
      state.framedelta = 1

   IF N_ELEMENTS(rate) NE 0 THEN BEGIN
      rate = 0 > rate < 100            ; In range?
      WIDGET_CONTROL, state.wFramesSpeedSlider, SET_VALUE = rate
   ENDIF ELSE $
      rate = 100

   IF rate EQ 100 THEN $
      state.delay = 0.0 $
   ELSE $
      state.delay = 4.0 / (1.0 + rate)

   SetMyBitmapButtons, state

   state.loop_start_t = SYSTIME(1)            ;Start of loop time
   WIDGET_CONTROL, wAnimateBase, TIMER=state.delay, EVENT_FUNC='CW_ANIMATE_EVNT'
 done:

   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY  ; Rewrite state
   SafeWSET, old_window, didit

END  ;  CW_ANIMATE_RUN

;***************************************************************************
PRO CW_ANIMATE_GETP, wAnimateBase, PIXMAPS, KILL_ANYWAY = kill_anyway
;***************************************************************************
; Return the vector of pixmap ID's associated with the animation widget in
; named variable PIXMAPSTATE. Frames without a pixmap contain a -1. This
; routine should not be called until all the frames are loaded, or the
; vector will not be complete. It should be called before the call to
; CW_ANIMATE_RUN.
;
; Note: Normally, the animation widget destroys its pixmaps when it is
;       destroyed. If this routine is called however, the pixmaps are not
;       destroyed. Cleanup becomes the responsibility of the caller.
;---------------------------------------------------------------------------

   wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
   WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY
   pwin = Temporary(*state.pwinHdl)
   pixmaps = pwin
   *state.pwinHdl = Temporary(pwin)
   IF KEYWORD_SET(kill_anyway) EQ 0 THEN $
      state.nokill_pixmaps = 1
   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY

END  ;  CW_ANIMATE_GETP

;***************************************************************************
PRO CW_ANIMATE_INITME, wAnimateBase, sizex, sizey, nframes, PIXMAPS=old_pixmaps
;***************************************************************************
; Initialize the draw window with new data.
;---------------------------------------------------------------------------

   Error_status = 0
   CATCH, Error_status  
   IF (Error_status NE 0) THEN BEGIN  
      rtrn = DIALOG_MESSAGE(!ERROR_STATE.MSG, /ERROR, /CENTER)
      CATCH, /CANCEL  
      RETURN
   ENDIF  

   wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
   WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY

   nparams = N_PARAMS()
   IF (nparams LT 3) OR (nparams GT 4) THEN $
      MESSAGE, 'Incorrect number of arguments'
   IF ~ (KEYWORD_SET(uval)) THEN $
      uval = 0

   ;------------------------------------------------------------------------
   ; Save the number of frames to animate in the animation structure.
   ;------------------------------------------------------------------------
   
   n = N_ELEMENTS(old_pixmaps)
   IF (n GT 0) THEN BEGIN
      nframes = n
      pwin = old_pixmaps
   ENDIF ELSE $
      pwin = REPLICATE(-1, nframes)            ; Array of window indices

   IF (nframes LE 1) THEN $
      MESSAGE, "Animations must have 2 or more frames"

   state.nframes = nframes
   
   ;------------------------------------------------------------------------
   ; Save the Pixmap array to animate in the animation structure.
   ;------------------------------------------------------------------------

   IF PTR_VALID(state.pwinHdl) THEN BEGIN
       ; Need to temporarily restore state since following routine uses it
      WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY
      CW_ANIMATE_CLEAN, wTopWorkBase
       ; Need structure back - This kills the widget uvalue
      WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY
   ENDIF ELSE BEGIN
      state.pwinHdl = PTR_NEW(pwin, /NO_COPY)
   ENDELSE

   WIDGET_CONTROL, state.wFramesWhichSlider, SET_SLIDER_MIN = 0, $
                   SET_SLIDER_MAX = nframes - 1

   ;------------------------------------------------------------------------
   ; Create the draw window's base widget and the draw window. Set the size
   ; of the draw window to the full size of the MISR image. Set the size of
   ; the viewport depending on size of screen and size of image.
   ;------------------------------------------------------------------------
                
   IF state.wDrawWindow NE 0 THEN BEGIN
      ; to avoid flash - only set the size if it changes
      IF (state.sizex NE sizex OR state.sizey NE sizey) THEN $
         WIDGET_CONTROL, state.wDrawWindow, XSIZE=sizex, YSIZE=sizey
   ENDIF ELSE BEGIN
      state.wDrawBase = WIDGET_BASE(wTopWorkBase, /COLUMN, TLB_SIZE_EVENTS=1, $
                                    UNAME="DRAW_BASE") ; Allow resizing

      x_fractionscreen = 0.95
      x_blackswathedge = 320 + (sizey / 14) ; ~= num blks * 36 pix/blk
      CntlGeom1 = WIDGET_INFO(state.wTopWorkBase, /GEOMETRY)
      y_scrollbarfudge = 2 * CntlGeom1.MARGIN
      CntlGeom1 = WIDGET_INFO(state.wControlBaseTop, /GEOMETRY)
      CntlGeom2 = WIDGET_INFO(state.wControlBaseBot, /GEOMETRY)
      y_topbotcontrols = CntlGeom1.SCR_YSIZE + CntlGeom2.SCR_YSIZE + 150
      CntlGeom1 = 0
      CntlGeom2 = 0

      viewportx = (!KON.Misc.ScreenX * x_fractionscreen) < $
                  (sizex - x_blackswathedge)
      viewporty = (!KON.Misc.ScreenY - y_topbotcontrols) < $
                  (sizey + y_scrollbarfudge)
      
      state.wDrawWindow = $
         WIDGET_DRAW(state.wDrawBase, XSIZE=sizex, YSIZE=sizey, $
                     X_SCROLL_SIZE=viewportx, Y_SCROLL_SIZE=viewporty, $
                     RETAIN=2, /BUTTON_EVENTS, KEYBOARD_EVENTS=1, $
                     TRACKING_EVENTS=1, UNAME="DRAW_WINDOW")
   ENDELSE
   
   ;------------------------------------------------------------------------
   ; Save the x and y dimensions of draw widget in the animation structure.
   ;------------------------------------------------------------------------

   state.sizex = sizex
   state.sizey = sizey

   ;------------------------------------------------------------------------
   ; Disable all controls until all frames are loaded.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, wAnimateBase, SENSITIVE = 0
   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY

   CATCH, /CANCEL  

END  ;  CW_ANIMATE_INITME

;***************************************************************************
PRO InitMyBitmapButtons
;***************************************************************************
; Setup the play reverse, pause, play forward and cycle pushbutton bitmaps.
; These variables reside in the "BitmapButtons" common block. Both a
; depressed (blk_) and a not-depressed version are needed for each button.
;---------------------------------------------------------------------------

   COMMON bitmap_btns, reversebutton, blk_reversebutton, pausebutton, $
                       blk_pausebutton, playbutton, blk_playbutton, $
                       cycleForwardBtn, blk_cycleForwardBtn

   compile_opt hidden

   reversebutton = $
           [[000B, 000B, 000B], [000B, 032B, 000B], [000B, 048B, 000B], $
            [000B, 056B, 000B], [000B, 060B, 000B], [000B, 046B, 000B], $
            [000B, 231B, 015B], [144B, 003B, 024B], [016B, 231B, 027B], $
            [080B, 238B, 027B], [208B, 060B, 026B], [208B, 056B, 026B], $
            [208B, 048B, 026B], [208B, 032B, 026B], [208B, 000B, 026B], $
            [208B, 000B, 026B], [208B, 255B, 027B], [016B, 000B, 024B], $
            [240B, 255B, 031B], [224B, 255B, 015B], [000B, 000B, 000B], $
            [000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B]]
   blk_reversebutton = $
           [[255B, 255B, 255B], [255B, 223B, 255B],[255B, 207B, 255B],$
            [255B, 199B, 255B], [255B, 195B, 255B], [255B, 209B, 255B], $
            [255B, 024B, 240B], [111B, 252B, 231B], [239B, 024B, 228B], $
            [175B, 017B, 228B], [047B, 195B, 229B], [047B, 199B, 229B], $
            [047B, 207B, 229B], [047B, 223B, 229B], [047B, 255B, 229B], $
            [047B, 255B, 229B], [047B, 000B, 228B], [239B, 255B, 231B], $
            [015B, 000B, 224B], [031B, 000B, 240B], [255B, 255B, 255B], $
            [255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B]]
   pausebutton = $
           [[000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B], $
            [192B, 195B, 003B], [192B, 194B, 002B], [192B, 194B, 002B], $
            [192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
            [192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
            [192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
            [192B, 194B, 002B], [192B, 194B, 002B], [192B, 194B, 002B], $
            [192B, 194B, 002B], [192B, 195B, 003B], [192B, 195B, 003B], $
            [000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B]]
   blk_pausebutton = $
           [[255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B],$
            [063B, 060B, 252B], [063B, 061B, 253B], [063B, 061B, 253B], $
            [063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
            [063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
            [063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
            [063B, 061B, 253B], [063B, 061B, 253B], [063B, 061B, 253B], $
            [063B, 061B, 253B], [063B, 060B, 252B], [063B, 060B, 252B], $
            [255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B]]
   playbutton = $
           [[000B, 000B, 000B], [000B, 004B, 000B], [000B, 012B, 000B], $
            [000B, 028B, 000B], [000B, 060B, 000B], [000B, 116B, 000B], $
            [240B, 231B, 000B], [024B, 192B, 009B], [216B, 231B, 008B], $
            [216B, 119B, 010B], [088B, 060B, 011B], [088B, 028B, 011B], $
            [088B, 012B, 011B], [088B, 004B, 011B], [088B, 000B, 011B], $
            [088B, 000B, 011B], [216B, 255B, 011B], [024B, 000B, 008B], $
            [248B, 255B, 015B], [240B, 255B, 007B], [000B, 000B, 000B], $
            [000B, 000B, 000B], [000B, 000B, 000B], [000B, 000B, 000B]]
   blk_playbutton = $
           [[255B, 255B, 255B], [255B, 251B, 255B], [255B, 243B, 255B],$
            [255B, 227B, 255B], [255B, 195B, 255B], [255B, 139B, 255B], $
            [015B, 024B, 255B], [231B, 063B, 246B], [039B, 024B, 247B], $
            [039B, 136B, 245B], [167B, 195B, 244B], [167B, 227B, 244B], $
            [167B, 243B, 244B], [167B, 251B, 244B], [167B, 255B, 244B], $
            [167B, 255B, 244B], [039B, 000B, 244B], [231B, 255B, 247B], $
            [007B, 000B, 240B], [015B, 000B, 248B], [255B, 255B, 255B], $
            [255B, 255B, 255B], [255B, 255B, 255B], [255B, 255B, 255B]]
  cycleForwardBtn = $
           [[000B, 000B, 000B], [000B, 000B, 000B], [000B, 128B, 000B], $
            [000B, 128B, 001B], [000B, 128B, 003B], [248B, 255B, 006B], $
            [008B, 000B, 012B], [008B, 000B, 024B], [248B, 255B, 012B], $
            [248B, 255B, 006B], [000B, 128B, 003B], [000B, 129B, 001B], $
            [128B, 129B, 000B], [192B, 001B, 000B], [096B, 255B, 015B], $
            [048B, 000B, 008B], [024B, 000B, 008B], [048B, 255B, 015B], $
            [096B, 255B, 015B], [192B, 001B, 000B], [128B, 001B, 000B], $
            [000B, 001B, 000B], [000B, 000B, 000B], [000B, 000B, 000B]]
   blk_cycleForwardBtn = $
           [[255B, 255B, 255B], [255B, 255B, 255B], [255B, 127B, 255B], $
            [255B, 127B, 254B], [255B, 127B, 252B], [007B, 000B, 249B], $
            [247B, 255B, 243B], [247B, 255B, 231B], [007B, 000B, 243B], $
            [007B, 000B, 249B], [255B, 127B, 252B], [255B, 126B, 254B], $
            [127B, 126B, 255B], [063B, 254B, 255B], [159B, 000B, 240B], $
            [207B, 255B, 247B], [231B, 255B, 247B], [207B, 000B, 240B], $
            [159B, 000B, 240B], [063B, 254B, 255B], [127B, 254B, 255B], $
            [255B, 254B, 255B], [255B, 255B, 255B], [255B, 255B, 255B]]

END  ;  InitMyBitmapButtons

;***************************************************************************
FUNCTION CW_ANIMATE, parent, sizex, sizey, nframes, add_edge, UVALUE=uval, $
                     PIXMAPS=old_pixmaps, CYCLE=cycle, NO_KILL=no_kill, $
                     DRAW=draw, OPEN_FUNC=open_func, UNAME=uname
;***************************************************************************
; Create the controls for the animation window and save data in window
; structure.
;---------------------------------------------------------------------------

   COMMON top_level_base, wTopWorkBase
   COMMON bitmap_btns, reversebutton, blk_reversebutton, pausebutton, $
                       blk_pausebutton, playbutton, blk_playbutton, $
                       cycleForwardBtn, blk_cycleForwardBtn
   COMMON coord_data, CoordStruct
   COMMON save_product, ProductList

   COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Error_status = 0
   CATCH, Error_status  
   IF (Error_status NE 0) THEN BEGIN  
      rtrn = DIALOG_MESSAGE(!ERROR_STATE.MSG, /ERROR, /CENTER)
      CATCH, /CANCEL  
      RETURN, -1
   ENDIF  

   scroll_val = add_edge

   ; Set the names of the operations that can be performed on the data.

   ;------------------------------------------------------------------------
   ; Create a list containing names and hierarchy descriptions for task menu.
   ;------------------------------------------------------------------------

   IF (!KON.Misc.MINX_PLATFORM EQ 1) THEN BEGIN
      main_str1 = '1\Task' + !KON.Misc.NewLine + 'Menu'
      main_str2 = '1\Data' + !KON.Misc.NewLine + 'Menu'
      help_str  = ' PDF '  + !KON.Misc.NewLine + ' Help'
   ENDIF ELSE BEGIN
      main_str1 = '1\Tasks'
      main_str2 = '1\Data Menu'
      help_str  = 'Help'
   ENDELSE

   IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
      OperationList = [main_str1, $
                         '0\PDF Help ...', $
                         '0\Save Session ...', $
                         '0\Restore Session ...', $
                         '1\Save Camera Image', $
                           '0\Save this Camera image for Google Earth ...', $
                           '0\Save this Camera image as GeoTIFF ...', $
                           '0\Save this Camera image as TIFF ...', $
                           '0\Save this Camera image as JPEG ...', $
                           '0\Save this Camera image as PNG ...', $
                           '0\Save this Camera image as GIF ...', $
                           '0\Save Camera images in 9 TIFF files ...', $
                           '0\Save Camera images in 9 JPEG files ...', $
;                           '0\Save 9 Camera images in 1 GIF file ...', $
                           '2\Save 9 Camera images in 1 MP4 file ...', $
;                         '0\Write MISR Data to Text File ...', $
                         '1\Correct Misregistration', $
                           '0\Warp Orbit 1 Cameras for Plumes CNTL-W', $
                           '0\Warp Orbit 2 An to Match Orbit 1 An ...', $
                           '0\Warp Orbit 1 and 2 Cameras to Match An', $
                           '2\Warp Current Camera to Match An ...', $
                         '1\Select Digitizing Tool', $
                           '0\Enable Digitizing ... CNTL-D', $
                           '0\Enable Deleting CNTL-R', $
                           '0\Enable Redigitizing ...', $
                           '0\Disable all Options CNTL-C', $
                           '0\Set Dig. Display Options ... CNTL-O', $
                           '2\Load Modis Fire Pixels ... CNTL-F', $
                         '0\Select Color Palette ... CNTL-P', $
                         '0\Set Swath Display Options ... CNTL-S', $
                         '0\Post Marker Pixels from File ...', $
                         '0\Red/Blue 3D ...', $
                         '0\MISR Vision RGB ...', $
                         '1\RGB: 2 Orbit Camera Differences', $
                           '0\Df Orbit Diff', $
                           '0\Cf Orbit Diff', $
                           '0\Bf Orbit Diff', $
                           '0\Af Orbit Diff', $
                           '0\An Orbit Diff', $
                           '0\Aa Orbit Diff', $
                           '0\Ba Orbit Diff', $
                           '0\Ca Orbit Diff', $
                           '2\Da Orbit Diff', $
                         '1\RGB: Edge Enhancers', $
                           '0\Roberts: DfAnDa', $
                           '0\Roberts: CfAnCa', $
                           '0\Sobel: DfAnDa', $
                           '2\Sobel: CfAnCa', $
                         '1\Camera/Band Combinations', $
                           '1\Gray: Band Differences for An', $
                             '0\ABS(Blue-Green)', $
                             '0\ABS(Blue-Red)', $
                             '2\ABS(Blue-NIR)', $
                           '1\Gray: Cam/An Ratios', $
                             '0\Df/An', $
                             '0\Cf/An', $
                             '0\Bf/An', $
                             '0\Af/An', $
                             '0\Aa/An', $
                             '0\Ba/An', $
                             '0\Ca/An', $
                             '2\Da/An', $
                           '1\Gray: Complementary Cam Ratios', $
                             '0\Df/Da', $
                             '0\Cf/Ca', $
                             '0\Bf/Ba', $
                             '2\Af/Aa', $
                           '1\RGB: Complementary Cam Ratios', $
                             '0\Df/Da, Cf/Ca, Bf/Ba', $
                             '2\Cf/Ca, Bf/Ba, Af/Aa', $
                           '1\Gray: Complementary Cam Diffs', $
                             '0\Df-Da', $
                             '0\Cf-Ca', $
                             '0\Bf-Ba', $
                             '2\Af-Aa', $
                           '1\Gray: Ratios of Diff Over Sum', $
                             '0\(Df-Da)/(Df+Da)', $
                             '0\(Cf-Ca)/(Cf+Ca)', $
                             '0\(Bf-Ba)/(Bf+Ba)', $
                             '2\(Af-Aa)/(Af+Aa)', $
                           '1\RGB: Adjacent Cam Diffs', $
                             '0\Df-Cf, Cf-Bf, Bf-Af', $
                             '0\Cf-Bf, Bf-Af, Af-An', $
                             '0\Ca-Ba, Ba-Aa, Aa-An', $
                             '2\Da-Ca, Ca-Ba, Ba-Aa', $
                           '1\RGB: Adjacent Cam Ratios', $
                             '0\Dx-Cx, Cx-Bx, Bx-Ax', $
                             '0\Cx-Bx, Bx-Ax, Ax-An', $
                             '0\Cx+Bx, Bx+Ax, Ax+An', $
                             '2\Dx+Cx, Cx+Bx, Bx+Ax', $
                         '2\', $
                       '2\']
   ENDIF ELSE BEGIN
      OperationList = [main_str1, $
                         '0\PDF Help ...', $
                         '0\Save Session ...', $
                         '0\Restore Session ...', $
                         '1\Save Camera Image', $
                           '0\Save this Camera image for Google Earth ...', $
                           '0\Save this Camera image as GeoTIFF ...', $
                           '0\Save this Camera image as TIFF ...', $
                           '0\Save this Camera image as JPEG ...', $
                           '0\Save this Camera image as PNG ...', $
                           '0\Save this Camera image as GIF ...', $
                           '0\Save Camera images in 9 TIFF files ...', $
                           '0\Save Camera images in 9 JPEG files ...', $
;                           '0\Save 9 Camera images in 1 GIF file ...', $
                           '2\Save 9 Camera images in 1 MP4 file ...', $
;                         '0\Write MISR Data to Text File ...', $
                         '1\Correct Misregistration', $
                           '0\Warp Orbit 1 Cameras for Plumes CNTL-W', $
                           '0\Warp Orbit 2 An to Match Orbit 1 An ...', $
                           '0\Warp Orbit 1 and 2 Cameras to Match An', $
                           '2\Warp Current Camera to Match An ...', $
                         '1\Select Digitizing Tool', $
                           '0\Enable Digitizing ... CNTL-D', $
                           '0\Enable Deleting CNTL-R', $
                           '0\Enable Redigitizing ...', $
                           '0\Disable all Options CNTL-C', $
                           '0\Set Dig. Display Options ... CNTL-O', $
                           '2\Load Modis Fire Pixels ... CNTL-F', $
                         '0\Select Color Palette ... CNTL-P', $
                         '0\Set Swath Display Options ... CNTL-S', $
                         '0\Post Marker Pixels from File ...', $
                         '0\Red/Blue 3D ...', $
                         '0\MISR Vision RGB ...', $
                         '1\Gray: 2 Orbit Camera Differences', $
                           '0\Df Orbit Diff', $
                           '0\Cf Orbit Diff', $
                           '0\Bf Orbit Diff', $
                           '0\Af Orbit Diff', $
                           '0\An Orbit Diff', $
                           '0\Aa Orbit Diff', $
                           '0\Ba Orbit Diff', $
                           '0\Ca Orbit Diff', $
                           '0\Da Orbit Diff', $
                           '0\Df-Da Orbit Diff - Red-NIR', $
                           '0\Cf-Ca Orbit Diff - Red-NIR', $
                           '0\Bf-Ba Orbit Diff - Red-NIR', $
                           '0\Af-Aa Orbit Diff - Red-NIR', $
                           '0\Aa-Af Orbit Diff - Red-NIR', $
                           '0\Ba-Bf Orbit Diff - Red-NIR', $
                           '0\Ca-Cf Orbit Diff - Red-NIR', $
                           '2\Da-Df Orbit Diff - Red-NIR', $
                         '0\Aerosol Classifier', $
                         '1\High Pass Filter', $
                           '0\Scaled RGB', $
                           '2\Blue/Red Ratio', $
                         '1\RGB: Edge Enhancers', $
                           '0\Roberts: DfAnDa', $
                           '0\Roberts: CfAnCa', $
                           '0\Sobel: DfAnDa', $
                           '2\Sobel: CfAnCa', $
                         '1\RGB: BRF Shape Characterization', $
                           '0\Mean, StdDev, Skewness (slow)', $
                           '0\Mean Df->Bf,Mean Af->Aa,Mean Ba->Da', $
                           '2\Quadratic Fit: A, B, C (VERY slow)', $
                         '1\Camera/Band Combinations', $
                           '1\Gray: Band Differences for An', $
                             '0\ABS(Blue-Green)', $
                             '0\ABS(Blue-Red)', $
                             '2\ABS(Blue-NIR)', $
                           '1\Gray: Cam/An Ratios', $
                             '0\Df/An', $
                             '0\Cf/An', $
                             '0\Bf/An', $
                             '0\Af/An', $
                             '0\Aa/An', $
                             '0\Ba/An', $
                             '0\Ca/An', $
                             '2\Da/An', $
                           '1\Gray: Complementary Cam Ratios', $
                             '0\Df/Da', $
                             '0\Cf/Ca', $
                             '0\Bf/Ba', $
                             '2\Af/Aa', $
                           '1\RGB: Complementary Cam Ratios', $
                             '0\Df/Da, Cf/Ca, Bf/Ba', $
                             '2\Cf/Ca, Bf/Ba, Af/Aa', $
                           '1\Gray: Complementary Cam Diffs', $
                             '0\Df-Da', $
                             '0\Cf-Ca', $
                             '0\Bf-Ba', $
                             '2\Af-Aa', $
                           '1\Gray: Ratios of Diff Over Sum', $
                             '0\(Df-Da)/(Df+Da)', $
                             '0\(Cf-Ca)/(Cf+Ca)', $
                             '0\(Bf-Ba)/(Bf+Ba)', $
                             '2\(Af-Aa)/(Af+Aa)', $
                           '1\RGB: Adjacent Cam Diffs', $
                             '0\Df-Cf, Cf-Bf, Bf-Af', $
                             '0\Cf-Bf, Bf-Af, Af-An', $
                             '0\Ca-Ba, Ba-Aa, Aa-An', $
                             '2\Da-Ca, Ca-Ba, Ba-Aa', $
                           '1\RGB: Adjacent Cam Ratios', $
                             '0\Dx-Cx, Cx-Bx, Bx-Ax', $
                             '0\Cx-Bx, Bx-Ax, Ax-An', $
                             '0\Cx+Bx, Bx+Ax, Ax+An', $
                             '2\Dx+Cx, Cx+Bx, Bx+Ax', $
                         '2\', $
                       '2\']
    ENDELSE

   ;------------------------------------------------------------------------
   ; Generate a menu containing the names and groups of MISR data products
   ; to display when the user clicks the Data Menu button. The menus above
   ; could be generalized in this way also, so changing a few parameters in
   ; the !KON and !VAR structures results in a different menu.
   ;------------------------------------------------------------------------

   pre_maj_str = '0\'
   pre_min_str = '1\'
   pre_end_str = '2\'
   old_field   = ''
   old_group   = ''
   irow        = 0

   ProductList = STRARR(N_ELEMENTS(!VAR.DataProd) + 2)
   ProductList[irow] = main_str2

   FOR irow=1,N_ELEMENTS(!VAR.DataProd)-2 DO BEGIN
      new_group = !VAR.DataProd[irow].Group
      new_field = !VAR.DataProd[irow].Field
      nxt_group = !VAR.DataProd[irow+1].Group
      
      IF (new_group EQ '' AND new_field EQ '') THEN BEGIN
         ProductList[irow] = pre_min_str + nxt_group
         old_group = new_group
         CONTINUE
      ENDIF
         
      IF (new_group EQ new_field) THEN BEGIN
         ProductList[irow] = pre_maj_str + new_field
         old_group = new_group
         CONTINUE
      ENDIF

      IF (new_group EQ nxt_group) THEN BEGIN
         ProductList[irow] = pre_maj_str + new_field
         old_group = new_group
         CONTINUE
      ENDIF ELSE BEGIN
         ProductList[irow] = pre_end_str + new_field
         old_group = new_group
         CONTINUE
      ENDELSE
   ENDFOR
   
   new_field = !VAR.DataProd[irow].Field
   ProductList[irow] = pre_end_str + new_field
   irow += 1
   ProductList[irow] = pre_end_str
   irow += 1
   ProductList[irow] = pre_end_str

   ;------------------------------------------------------------------------
   ; Set the bitmaps for the bitmap buttons.
   ;------------------------------------------------------------------------

   InitMyBitmapButtons

   nparams = N_PARAMS()
   IF ((nparams LT 4) OR (nparams GT 6)) THEN $
      MESSAGE, 'Incorrect number of arguments'

   IF ~ (KEYWORD_SET(uval))      THEN uval = 0
   IF ~ (KEYWORD_SET(open_func)) THEN open_func = 0
   uname = 'ANIMATE_BASE'

   ;------------------------------------------------------------------------
   ; Create the base descendants of the top base.
   ;------------------------------------------------------------------------

   pad_x = 1
   pad_y = 1

   wVeryTop = parent
   wAnimateBase = WIDGET_BASE(wVeryTop, /COLUMN, /FRAME, TLB_SIZE_EVENTS=1, $
                              EVENT_PRO='EventHandler_wAnimate', UNAME=uname, $
                              XPAD=pad_x)
   wControlBaseTop = WIDGET_BASE(wAnimateBase, /ROW, /ALIGN_LEFT, $
                                 TLB_SIZE_EVENTS=1, UNAME='CONTROL_BASE_TOP', $
                                 XPAD=pad_x)
   wTopWorkBase = WIDGET_BASE(wAnimateBase, /COLUMN, /ALIGN_LEFT, /FRAME, $
                              UNAME='TOP_WORK_BASE', XPAD=pad_x)
   wControlBaseBot = WIDGET_BASE(wAnimateBase, /ROW, /ALIGN_LEFT, $
                                 TLB_SIZE_EVENTS=1, UNAME='CONTROL_BASE_BOT', $
                                 XPAD=pad_x)

   ;------------------------------------------------------------------------
   ; Animation controls.
   ;------------------------------------------------------------------------

   wVCRButtonBase = WIDGET_BASE(wControlBaseTop, /ROW, XPAD=pad_x)
   wCyclePlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = cycleForwardBtn, $
                                    UNAME=uname+'_CYCLEPLAY')
   wPauseButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = blk_pausebutton, $
                                UNAME=uname+'_PAUSE')
   wPlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = playbutton, $
                               UNAME=uname+'_PLAY')
   wReversePlayButton = WIDGET_BUTTON(wVCRButtonBase, VALUE = reversebutton, $
                                      UNAME=uname+'_REVERSE')

   currentAction = wPauseButton
   currentBitmap = pausebutton

   wSpeedBase = WIDGET_BASE(wControlBaseTop, /COLUMN, XPAD=pad_x, YPAD=0)
   wFramesSpeedBase = WIDGET_BASE(wSpeedBase, TITLE = "", /COLUMN, /FRAME, $
                                  XPAD=pad_x, YPAD=0)
   wFramesPerSecBase = WIDGET_BASE(wFramesSpeedBase, /ROW, XPAD=pad_x, YPAD=0)
   wFramesPerSecLabel = WIDGET_LABEL(wFramesPerSecBase, VALUE='Frames/Sec: 00')
   wFramesSpeedSlider = WIDGET_SLIDER(wFramesSpeedBase, /DRAG, $
                                      VALUE=99, MINIMUM=0, MAXIMUM=99, $
                                      /SUPPRESS_VALUE, $
                                      UNAME=uname+'_FRAMESSPEED')

   ;------------------------------------------------------------------------
   ; Camera controls.
   ;------------------------------------------------------------------------

   wCamsBase = WIDGET_BASE(wControlBaseTop, /COLUMN, XPAD=pad_x, YPAD=0)
   wFramesCamsBase = WIDGET_BASE(wCamsBase, TITLE = '', /COLUMN, /FRAME, $
                                 XPAD=pad_x, YPAD=0)
   wFramesByCamBase = WIDGET_BASE(wFramesCamsBase, /ROW, XPAD=pad_x, YPAD=0)
   wFramesByCamLabel = WIDGET_LABEL(wFramesByCamBase, VALUE='Camera: OP')
   wFramesWhichSlider = WIDGET_SLIDER(wFramesCamsBase, /DRAG, $
                                      VALUE=0, MINIMUM=0, MAXIMUM=nframes-1, $
                                      /SUPPRESS_VALUE, SENSITIVE=0, $
                                      UNAME=uname+'_FRAMESINDICATOR')

   IF (nframes EQ 19) THEN BEGIN
      wButtonBase1 = WIDGET_BASE(wControlBaseTop, /ROW, XPAD=0, YPAD=0)
      wButtonIndicatorBase1 = WIDGET_BASE(wButtonBase1, ROW=2, TITLE='', $
                                          /EXCLUSIVE, XPAD=0, YPAD=0)
      wAnimateButton1 = WIDGET_BUTTON(wButtonIndicatorBase1, /ALIGN_LEFT,$
                               VALUE='Orbit 1', UNAME=uname+'_ButtonSINDICATOR')
      wAnimateButton2 = WIDGET_BUTTON(wButtonIndicatorBase1, /ALIGN_LEFT,$
                               VALUE='Orbit 2', UNAME=uname+'_ButtonSINDICATOR')
      wAnimateButton3 = WIDGET_BUTTON(wButtonIndicatorBase1, /ALIGN_LEFT,$
                               VALUE='Both', UNAME=uname+'_ButtonSINDICATOR')
      WIDGET_CONTROL, wAnimateButton1, /SET_BUTTON
   ENDIF ELSE BEGIN
      wAnimateButton1 = -1
      wAnimateButton2 = -1
      wAnimateButton3 = -1
   ENDELSE

   ;------------------------------------------------------------------------
   ; Task menu.
   ;------------------------------------------------------------------------

   wMenuBase = WIDGET_BASE(wControlBaseTop, XPAD=pad_x)
   wFramesMenuList = CW_PDMENU(wMenuBase, OperationList, /RETURN_NAME, $
                               UNAME=uname+'_FRAMESmenu')

   ;------------------------------------------------------------------------
   ; BRF controls.
   ;------------------------------------------------------------------------

   wBrfBase = WIDGET_BASE(wControlBaseTop, /FRAME, /COLUMN, /ALIGN_TOP, $
                          /BASE_ALIGN_TOP, XPAD=pad_x, YPAD=0)
   wBrfBase1 = WIDGET_BASE(wBrfBase, /ROW, /NONEXCLUSIVE, /BASE_ALIGN_TOP, $
                           XPAD=pad_x, YPAD=0)
   wFramesBrfButton = WIDGET_BUTTON(wBrfBase1, VALUE="BRF Analysis", $
                                    UNAME=uname+'_FRAMESbrf', /ALIGN_LEFT)
   wBrfParamsButton = WIDGET_BUTTON(wBrfBase, VALUE="BRF Plot Params", $
                                    SENSITIVE=0, /NO_RELEASE)

   ;------------------------------------------------------------------------
   ; Overposting toggle buttons.
   ;------------------------------------------------------------------------

   xoffcrds = [0, 85, 175, 265]
   yoffcrds = [0, 22]

   wChkBoxBase1 = WIDGET_BASE(wControlBaseTop, /FRAME, XPAD=pad_x, YPAD=0)

   wDotsBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                            XOFFSET=xoffcrds[0], YOFFSET=yoffcrds[0], $
                            XPAD=pad_x, YPAD=0)
   wFramesDotsButton = WIDGET_BUTTON(wDotsBase, VALUE="Fixed Grid", $
                            UNAME=uname+'_FRAMESDots', SENSITIVE=1)
      
   wCountryBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                            XOFFSET=xoffcrds[1], YOFFSET=yoffcrds[0], $
                            XPAD=pad_x, YPAD=0)
   wFramesCountryBtn = WIDGET_BUTTON(wCountryBase, VALUE="Geography", $
                            UNAME=uname+'_FRAMESGeog', SENSITIVE=1)
                            
   wLLgridBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                            XOFFSET=xoffcrds[2], YOFFSET=yoffcrds[0], $
                            XPAD=pad_x, YPAD=0)
   wFramesLLgridBtn = WIDGET_BUTTON(wLLgridBase, VALUE="Geo Grid",$
                            UNAME=uname+'_FRAMESLatLon', SENSITIVE=1)
                            
   wHolesBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                            XOFFSET=xoffcrds[3], YOFFSET=yoffcrds[0], $
                            XPAD=pad_x, YPAD=0)
   wFramesHolesButton = WIDGET_BUTTON(wHolesBase, VALUE="Terr Holes", $
                            UNAME=uname+'_FRAMESHoles', SENSITIVE=1)
      
   wLinesBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                           XOFFSET=xoffcrds[0], YOFFSET=yoffcrds[1], $
                           XPAD=pad_x, YPAD=0)
   wFramesLinesButton = WIDGET_BUTTON(wLinesBase, VALUE="Digitizing", $
                           UNAME=uname+'_FRAMESLines', SENSITIVE=0)

   wPixColorBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                           XOFFSET=xoffcrds[1], YOFFSET=yoffcrds[1], $
                           XPAD=pad_x, YPAD=0)
   wFramesPixColorBtn = WIDGET_BUTTON(wPixColorBase, VALUE="Data Values", $
                           UNAME=uname+'_FRAMESPixColor', SENSITIVE=0)

   wFireBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                           XOFFSET=xoffcrds[2], YOFFSET=yoffcrds[1], $
                           XPAD=pad_x, YPAD=0)
   wFramesFireButton = WIDGET_BUTTON(wFireBase, VALUE="Fire Pixels", $
                           UNAME=uname+'_FRAMESFire', SENSITIVE=0)
                           
   wMarkerBase = WIDGET_BASE(wChkBoxBase1, /ALIGN_LEFT, /NONEXCLUSIVE, $
                           XOFFSET=xoffcrds[3], YOFFSET=yoffcrds[1], $
                           XPAD=pad_x, YPAD=0)
   wFramesMarkerButton = WIDGET_BUTTON(wMarkerBase, VALUE="Markers", $
                           UNAME=uname+'_FRAMESMarker', SENSITIVE=0)
         
   ;------------------------------------------------------------------------
   ; Select MISR swath data product menu and execute button.
   ;------------------------------------------------------------------------

   wProductBase = WIDGET_BASE(wControlBaseTop, /FRAME, /ROW, XPAD=pad_x, $
                              YPAD=0)
   wFramesProductMenu = CW_PDMENU(wProductBase, ProductList, /RETURN_NAME, $
                                  UNAME=uname+'_PRODUCTmenu')

   btn_val = (!KON.Misc.MINX_PLATFORM EQ 1) ? $
      ' Press and Hold ' + !KON.Misc.NewLine + 'to Display Data' : $
      ' Depress to Show '

   wFramesShowProdBtn = WIDGET_BUTTON(wProductBase, /PUSHBUTTON_EVENTS, $
                               UNAME=uname+'_FRAMESProduct', VALUE=btn_val, $
                               /FRAME, SENSITIVE=0)
   CntlGeom = WIDGET_INFO(wFramesShowProdBtn, /GEOMETRY)
   WIDGET_CONTROL, wFramesShowProdBtn, SCR_YSIZE=CntlGeom.SCR_YSIZE+5

   wButtonBase2 = WIDGET_BASE(wControlBaseTop, /COLUMN, /ALIGN_LEFT, $
                              XPAD=pad_x, YPAD=0)

   ;-----------------------------------------------------------------------
   ; PDF Help button for animation window.
   ;-----------------------------------------------------------------------

   wHelpButton = WIDGET_BUTTON(wControlBaseTop, VALUE=help_str, /FRAME, $
                               UNAME=uname+'_HELP', XSIZE=45, YSIZE=30)

   ;-----------------------------------------------------------------------
   ; This is not used.
   ;-----------------------------------------------------------------------

   IF (KEYWORD_SET(open_func)) THEN BEGIN
      wOpenButton = WIDGET_BUTTON(wButtonBase2, VALUE='Open...', $
          UNAME=uname+'_OPEN')
   ENDIF ELSE BEGIN
      wOpenButton = 0
   ENDELSE

   IF KEYWORD_SET(no_kill) THEN wEndAnimationButton = 0L

   ;-----------------------------------------------------------------------
   ; Bottom panel of controls.
   ;-----------------------------------------------------------------------

   wFramesBase = WIDGET_BASE(wControlBaseBot, /ROW, /FRAME, XPAD=pad_x)

   ;------------------------------------------------------------------------
   ; Band display controls.
   ;------------------------------------------------------------------------

   wFramesBandsBase = WIDGET_BASE(wFramesBase, /COLUMN, XPAD=pad_x, YPAD=0)

   wBandsBase1 = WIDGET_BASE(wFramesBandsBase, /ROW, /BASE_ALIGN_TOP, $
                             XPAD=pad_x, YPAD=0)

   wBandsBaseLabel = WIDGET_LABEL(wBandsBase1, VALUE=' Color Controls:    ')
   
   tooltip_str = 'When depressed, all camera and band RGB colors are scaled ' + $
                 'to the same BRF range. Otherwise, each camera will be ' + $
                 'scaled independently.'
   wScalingMethodBase = WIDGET_BASE(wBandsBase1, /NONEXCLUSIVE, YSIZE=20, $
                           /ROW, /BASE_ALIGN_TOP, /ALIGN_TOP, XPAD=pad_x, YPAD=0)
   wScalingMethodBtn = WIDGET_BUTTON(wScalingMethodBase, VALUE="Equalized Color", $
                        SENSITIVE=0, TOOLTIP=tooltip_str, /ALIGN_LEFT)
      
   wBandsBaseBase = WIDGET_BASE(wBandsBase1, /ROW, /BASE_ALIGN_TOP, /ALIGN_TOP, $
                                XPAD=pad_x, YPAD=0)
   wBandsBase2 = WIDGET_BASE(wFramesBandsBase, /EXCLUSIVE, /ROW, /BASE_ALIGN_TOP, $
                             XPAD=pad_x, YPAD=0)

   wBandsRGB = WIDGET_BUTTON(wBandsBase2, VALUE='RGB', /NO_RELEASE, /ALIGN_LEFT, $
       SENSITIVE=0, TOOLTIP='Display Red/Green/Blue MISR bands as RGB color.')
 
   wBandsBlu = WIDGET_BUTTON(wBandsBase2, VALUE='Blu', /NO_RELEASE, /ALIGN_LEFT, $
       SENSITIVE=0, TOOLTIP='Display blue MISR band in grayscales.')

   wBandsGrn = WIDGET_BUTTON(wBandsBase2, VALUE='Grn', /NO_RELEASE, /ALIGN_LEFT, $
       SENSITIVE=0, TOOLTIP='Display green MISR band in grayscales.')

   wBandsRed = WIDGET_BUTTON(wBandsBase2, VALUE='Red', /NO_RELEASE, /ALIGN_LEFT, $
       SENSITIVE=0, TOOLTIP='Display red MISR band in grayscales.')

   wBandsNIR = WIDGET_BUTTON(wBandsBase2, VALUE='NIR', /NO_RELEASE, /ALIGN_LEFT, $
       SENSITIVE=0, TOOLTIP='Display NIR MISR band in grayscales.')

   active_btn = (CoordStruct.(0).num_band EQ 1) ? wBandsRed : wBandsRGB

   WIDGET_CONTROL, active_btn, /SET_BUTTON

   ;------------------------------------------------------------------------
   ; Color controls.
   ;------------------------------------------------------------------------

   wFramesRGBBase = WIDGET_BASE(wFramesBase, /ROW, XPAD=pad_x, YPAD=0)

   wFramesBtnBase = WIDGET_BASE(wFramesRGBBase, /COLUMN, /BASE_ALIGN_TOP, $
                                XPAD=pad_x, YPAD=0)

   wBrightnessBase = WIDGET_BASE(wFramesRGBBase, /COLUMN, XPAD=pad_x, $
                                 YPAD=0)
   wPcntBrightnessBase = WIDGET_BASE(wBrightnessBase, TITLE="", /COLUMN, $
                                     XPAD=pad_x, YPAD=0)
   wBrightnessCntlBase = WIDGET_BASE(wPcntBrightnessBase, /ROW, XPAD=pad_x, $
                                     YPAD=0)
   wBrightnessLabel = WIDGET_LABEL(wBrightnessCntlBase, VALUE="Brightness:")
   str = STRING(FORMAT='(F4.2)', 0.5)
   wBrightnessValue = WIDGET_LABEL(wBrightnessCntlBase, VALUE=str)
   wBrightCntlSlider = WIDGET_SLIDER(wPcntBrightnessBase, VALUE=50, $
                              MINIMUM=0, MAXIMUM=100, SCROLL=10, $
                              SENSITIVE=0, /SUPPRESS_VALUE, $
                              UNAME=uname+'_FRAMESBrightCntl')
      
   wContrastBase = WIDGET_BASE(wFramesRGBBase, /COLUMN, XPAD=pad_x, $
                               YPAD=0)
   wPcntContrastBase = WIDGET_BASE(wContrastBase, TITLE="", /COLUMN, $
                                   XPAD=pad_x, YPAD=0)
   wContrastCntlBase = WIDGET_BASE(wPcntContrastBase, /ROW, XPAD=pad_x, $
                                   YPAD=0)
   wContrastLabel = WIDGET_LABEL(wContrastCntlBase, VALUE="Contrast:")
   str = STRING(FORMAT='(F4.2)', 0.5)
   wContrastValue = WIDGET_LABEL(wContrastCntlBase, VALUE=str)
   wContrastCntlSlider = WIDGET_SLIDER(wPcntContrastBase, $
                                MINIMUM=0, MAXIMUM=100, SCROLL=10, $
                                VALUE=50, SENSITIVE=0, /SUPPRESS_VALUE, $
                                UNAME=uname+'_FRAMESContrastCntl')

   ;-----------------------------------------------------------------------
   ; Bottom coordinate display boxes.
   ;-----------------------------------------------------------------------

   wSeparator1 = WIDGET_LABEL(wControlBaseBot, VALUE = ' ')

   wControlCoordBase = WIDGET_BASE(wControlBaseBot, TITLE = "", /ROW, /FRAME)

   wLabelbase = WIDGET_BASE(wControlCoordBase, /COLUMN, XPAD=pad_x, YPAD=5)
   wFramesCoordLabel1 = WIDGET_LABEL(wLabelBase, VALUE="Cursor ")
   wFramesCoordLabel2 = WIDGET_LABEL(wLabelBase, VALUE="Coords:")

   wCoordWndwBase = WIDGET_BASE(wControlCoordBase, /COLUMN, XSIZE=88, $
                                XPAD=pad_x, YPAD=0)
   wFramesCoordWndwLabel = WIDGET_LABEL(wCoordWndwBase, VALUE="Pixel x/y:")
   wFramesCoordWndwText = WIDGET_TEXT(wCoordWndwBase, VALUE = '0', $
                                      UNAME=uname+'_FRAMESCoordCntl')

   wCoordSomBase = WIDGET_BASE(wControlCoordBase, /COLUMN, XSIZE=120, $
                               XPAD=pad_x, YPAD=0)
   wFramesCoordSomLabel = WIDGET_LABEL(wCoordSomBase, VALUE = "Block/Across/Along:")
   wFramesCoordSomText = WIDGET_TEXT(wCoordSomBase, VALUE = '0', $
                                     UNAME=uname+'_FRAMESCoordCntl')

   wCoordGeoBase = WIDGET_BASE(wControlCoordBase, /COLUMN, XSIZE=128, $
                               XPAD=pad_x, YPAD=0)
   wFramesCoordGeoLabel = WIDGET_LABEL(wCoordGeoBase, VALUE="Longitude/Latitude:")
   wFramesCoordGeoText = WIDGET_TEXT(wCoordGeoBase, VALUE = '0.0', $
                                     UNAME=uname+'_FRAMESCoordCntl')

   wCoordDataBase = WIDGET_BASE(wControlCoordBase, /COLUMN, XSIZE=160, $
                                XPAD=pad_x, YPAD=0)
   pix_text = (CoordStruct.(0).num_band EQ 1) ? 'Pixel BRF Value (Red):' : $
                                                'Pixel BRF Values (RGBN):'
   wFramesCoordDataLabel = WIDGET_LABEL(wCoordDataBase,VALUE=pix_text)
   wFramesCoordDataText = WIDGET_TEXT(wCoordDataBase, VALUE = '0', $
                                      UNAME=uname+'_FRAMESCoordCntl')

   ;-----------------------------------------------------------------------
   ; Exit control for animation window.
   ;-----------------------------------------------------------------------

   wEndAnimationButton = WIDGET_BUTTON(wControlBaseBot, VALUE='Exit', $
                         UNAME=uname+'_ENDANIMATION', /ALIGN_CENTER, $
                         /FRAME, XSIZE=50, YSIZE=50)

   IF (N_ELEMENTS(draw) EQ 1) THEN wDrawWindow = draw $
   ELSE wDrawWindow = 0

   ;-----------------------------------------------------------------------
   ; Set the event handler function. This cluster does not get or set a
   ; value. Make sure it lingers so the cleanup routine can get at its state.
   ;-----------------------------------------------------------------------

   WIDGET_CONTROL, wAnimateBase, SET_UVALUE = uval, $
                   EVENT_FUNC = 'CW_ANIMATE_EVNT', /DELAY_DESTROY

   ;-----------------------------------------------------------------------
   ; This struct gets stuffed into the uval of first child of wAnimateBase
   ; which is the Draw Window.
   ;-----------------------------------------------------------------------

   state_str = { $
      wVeryTop        : wVeryTop, $        ; Topmost window
      wAnimateBase    : wAnimateBase, $    ; Animation base
      wTopWorkBase    : wTopWorkBase, $    ; Base containing draw and control bases
      wDrawBase       : 0, $               ; Base for drawing
      wControlBaseTop : wControlBaseTop, $ ; Base for top group of controls
      wControlBaseBot : wControlBaseBot, $ ; Base for bottom group of controls
      wDrawWindow     : wDrawWindow, $     ; Draw widget for animation
      
      wEndAnimationButton : wEndAnimationButton, $  ; End button
      wOpenButton         : wOpenButton, $          ; Open file button
      open_func           : open_func, $            ; Open file function
      wPauseButton        : wPauseButton, $         ; Stop (pause) button
      wPlayButton         : wPlayButton, $          ; Forward button
      wReversePlayButton  : wReversePlayButton, $   ; Reverse button
      wCyclePlayButton    : wCyclePlayButton, $     ; Cycle forward button
      wFramesSpeedSlider  : wFramesSpeedSlider, $   ; Speed selection slider
      wFramesPerSecLabel  : wFramesPerSecLabel, $   ; Animation rate display
      wFramesWhichSlider  : wFramesWhichSlider,$    ; Frame selection slider
      wFramesByCamLabel   : wFramesByCamLabel, $    ; Alpha camera name label
      wAnimateButton1     : wAnimateButton1, $      ; Animate orbit 1
      wAnimateButton2     : wAnimateButton2, $      ; Animate orbit 2
      wAnimateButton3     : wAnimateButton3, $      ; Animate both orbits
      animatebtn          : 1, $                    ; Which radio button active
      wBandsRGB           : wBandsRGB, $            ; Display RGB colors
      wBandsBlu           : wBandsBlu, $            ; Display blue in grayscale
      wBandsGrn           : wBandsGrn, $            ; Display green in grayscale
      wBandsRed           : wBandsRed, $            ; Display red in grayscale
      wBandsNIR           : wBandsNIR, $            ; Display NIR in grayscale
      wScalingMethodBtn   : wScalingMethodBtn, $    ; Method of scaling colors
      wBrightnessLabel    : wBrightnessLabel, $     ; Brightness slider name
      wBrightCntlSlider   : wBrightCntlSlider, $    ; Brightness slider
      wBrightnessValue    : wBrightnessValue, $     ; Brightness slider value
      wContrastLabel      : wContrastLabel, $       ; Contrast/NIR:Green slider name
      wContrastCntlSlider : wContrastCntlSlider, $  ; Contrast/NIR:Green slider
      wContrastValue      : wContrastValue, $       ; Contrast/NIR:Green slider value
      wFramesMenuList     : wFramesMenuList, $      ; Select operation on data
      wFramesBrfButton    : wFramesBrfButton, $     ; Enable/disable BRF analysis
      wBrfParamsButton    : wBrfParamsButton, $     ; Specify BRF plot window params
      wFramesDotsButton   : wFramesDotsButton, $    ; Show reference dots check box
      wFramesCountryBtn   : wFramesCountryBtn, $    ; Show country boundaries check box
      wFramesLLgridBtn    : wFramesLLgridBtn, $     ; Show lat/lon grid check box
      wFramesHolesButton  : wFramesHolesButton, $   ; Show terrain holes check box
      wFramesLinesButton  : wFramesLinesButton, $   ; Show drawn lines check box
      wFramesPixColorBtn  : wFramesPixColorBtn, $   ; Show map colors check box
      wFramesFireButton   : wFramesFireButton, $    ; Show MODIS fire check box
      wFramesMarkerButton : wFramesMarkerButton, $  ; Show Marker points check box
      wFramesProductMenu  : wFramesProductMenu, $   ; Select MISR product menu
      wFramesShowProdBtn  : wFramesShowProdBtn, $   ; Show MISR product show button
      wHelpButton         : wHelpButton, $          ; Show Animation Window PDF Help button
      wFramesCoordWndwText: wFramesCoordWndwText, $ ; Display window for pixel coords
      wFramesCoordSomText : wFramesCoordSOMText, $  ; Display window for MISR SOM coords
      wFramesCoordGeoText : wFramesCoordGeoText, $  ; Display window for long/lat coords
      wFramesCoordDataText: wFramesCoordDataText, $ ; Display window for pixel data value
      wButtonBase2        : wButtonBase2, $         ; Parent base of button that controls colors

      mousebtnstatus : 0, $             ; mouse button is down(1) or not(0)
      currentAction  : currentAction, $ ; current action button id
      currentBitmap  : currentBitmap, $ ; current button bitmap
      draw_win       : -1, $            ; index of window where drawing is done
      sizex          : 0, $             ; X dimensions of draw widget
      sizey          : 0, $             ; Y dimensions of draw widget
      add_edge       : scroll_val, $    ; padding to avoid edge wrapping after warp
      nframes        : 0, $             ; # of animation frames w/ images to copy to draw_win
                                        ;   (10 or 19): (1 or 2 orbits) * 9 + work window
      curframe       : 1, $             ; current animation frame (0 -> 18)
      begframe       : 1, $             ; first frame to display
      endframe       : 9, $             ; last frame to display
      framedelta     : 0, $             ; # of frames to step over
      loop_start_t   : 0.0D, $          ; system time at start: for animation loop rate
      delay          : 0.0D, $          ; time delay between frame displays
      cycle     : KEYWORD_SET(cycle), $ ; NE 0 to cycle or animate
      zoom_event_ptr : PTR_NEW(), $     ; last event passed by zoom event.
      showBRFwndw    : 0, $             ; 1 -> draw BRF window on mouse click
      dotdist        : 64,$             ; pixel offset between reference marks
      showdots       : 0, $             ; 1 -> show reference marks in image window
      showcountry    : 0, $             ; 1 -> show country boundaries in image window
      showLLgrid     : 0, $             ; 1 -> show lat/lon grid in image window
      showobjects    : 0, $             ; 1 -> show drawn lines in image window
      showmapclr     : 0, $             ; 1 -> show contoured regions in image window
      showmarker     : 0, $             ; 1 -> show user's marker pts in image window
      showfire       : 0, $             ; 1 -> show MODIS fires in image window
      nokill_pixmaps : 0, $             ; TRUE if pixmaps preserved on kill
      pwinHdl        : PTR_NEW() $      ; handle to the Pixmap array
}
   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state_str

   ;-----------------------------------------------------------------------
   ; When the child holding the state gets killed, have a cleanup procedure
   ; called to mop up.
   ;-----------------------------------------------------------------------

   WIDGET_CONTROL, wTopWorkBase, KILL_NOTIFY = 'CW_ANIMATE_CLEAN'

   CW_ANIMATE_INITME, wAnimateBase, sizex, sizey, nframes, PIXMAPS=old_pixmaps

   CATCH, /CANCEL  

   RETURN, wAnimateBase

END  ;  CW_ANIMATE
