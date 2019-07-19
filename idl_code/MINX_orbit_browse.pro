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
PRO OrbitBrowse_event, event
;***************************************************************************
; Event-handler routine.
;---------------------------------------------------------------------------

COMPILE_OPT hidden

COMMON orbit_image, OrbitImage_SizeX, OrbitImage_SizeY, NBLOCK_ORBIT, $
                    BLOCK_OFFSETS, ORBIT_WINDOW, DoneButton

  ;-------------------------------------------------------------------------
  ; Position the scroll bar so the top right of the view window is visible.
  ; This is so user is not confused by a blank window when many blocks are
  ; loaded.
  ;-------------------------------------------------------------------------

  IF (event.id EQ 1) THEN BEGIN
     WIDGET_CONTROL, widget, SET_DRAW_VIEW=[0, 0]
  ENDIF

  ;-------------------------------------------------------------------------
  ; Check the event type. If the event is a viewport event (type 3), redraw
  ; the image in the viewport using the new X and Y coordinates contained in
  ; the event structure.
  ;-------------------------------------------------------------------------

  IF (event.id EQ DoneButton) THEN BEGIN
      WIDGET_CONTROL, event.top, /DESTROY
      SafeWDELETE, ORBIT_WINDOW, didit
      ORBIT_WINDOW = -1
      RETURN
   ENDIF

END  ;  OrbitBrowse_event

;***************************************************************************
PRO OrbitBrowse, PathNum, OrbitNum, CamName, EorT, RorN
;***************************************************************************
; Widget creation routine.
; EorT: 0 -> ellipsoid; 1 -> terrain
; RorN: 0 -> NIR Green Blue; 1 -> Red Green Blue
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orbit_image, OrbitImage_SizeX, OrbitImage_SizeY, NBLOCK_ORBIT, $
                    BLOCK_OFFSETS, ORBIT_WINDOW, DoneButton

  ;-------------------------------------------------------------------------
  ; Create the base widget.
  ;-------------------------------------------------------------------------

  wndw_name = $
       'Path: ' + STRTRIM(STRING(PathNum),2) + $
       ', Orbit: ' + STRTRIM(STRING(OrbitNum[0]),2) + $
         (EorT ? ', TERRAIN' : ', ELLIPSOID') + $
       ', Camera: ' + STRTRIM(STRING(CamName),2) + $
       ', Bands: ' + (RorN ? 'Red/Green/Blue' :'NIR/Green/Blue')

  base = WIDGET_BASE(TITLE=wndw_name, /COLUMN)

  ;-------------------------------------------------------------------------
  ; Create the draw widget.
  ;-------------------------------------------------------------------------

  Xsize = OrbitImage_SizeX
  Ysize = OrbitImage_SizeY

  sizexx = FIX((!KON.Misc.ScreenX * 0.90) < Xsize)
  sizeyy = FIX((!KON.Misc.ScreenY * 0.90) < Ysize)

  IF (sizexx GT Xsize OR (sizeyy+5) GT Ysize) THEN BEGIN
     draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=Xsize, YSIZE=Ysize)
  ENDIF ELSE BEGIN
     draw = WIDGET_DRAW(base, RETAIN=2, $
                        X_SCROLL_SIZE=sizexx, Y_SCROLL_SIZE=sizeyy, $
                        XSIZE=Xsize, YSIZE=Ysize, /SCROLL)
  ENDELSE

  ;-------------------------------------------------------------------------
  ; Create button to enable destroying window.
  ;-------------------------------------------------------------------------

  DoneButton = WIDGET_BUTTON(base, VALUE='Exit', /ALIGN_CENTER, $
                             XSIZE=100)

  ;-------------------------------------------------------------------------
  ; Realize the widgets.
  ;-------------------------------------------------------------------------

  WIDGET_CONTROL, base, /REALIZE

  ;-------------------------------------------------------------------------
  ; Retrieve the window ID from the draw widget.
  ;-------------------------------------------------------------------------

  WIDGET_CONTROL, draw, GET_VALUE=drawID

  ;-------------------------------------------------------------------------
  ; Position the scroll bar so the top right of the view window is visible.
  ; This is so user is not confused by a blank window when many blocks are
  ; loaded.
  ;-------------------------------------------------------------------------

  WIDGET_CONTROL, draw, SET_DRAW_VIEW=[Xsize-sizexx, Ysize-sizeyy]

  ;-------------------------------------------------------------------------
  ; Set the draw widget as the current drawable area.
  ;-------------------------------------------------------------------------

  SafeWSET, drawID, didit

  ;-------------------------------------------------------------------------
  ; Load the image.
  ;-------------------------------------------------------------------------

  CATCH, error_status
  IF error_status NE 0 THEN BEGIN
     mssg = ['The You do not have enough memory to', $
             'load this many blocks. Try with fewer.']
     rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
     CATCH, /CANCEL
     GOTO, cleanit2
  ENDIF

  DEVICE, COPY = [0, 0, Xsize, Ysize, 0, 0, ORBIT_WINDOW]

  CATCH, /CANCEL

  ;-------------------------------------------------------------------------
  ; Call XMANAGER to manage the widgets.
  ;-------------------------------------------------------------------------

  XMANAGER, 'OrbitBrowse', base

cleanit2:
  SafeWDELETE, ORBIT_WINDOW, didit
  ORBIT_WINDOW = -1

END  ;  OrbitBrowse

;***************************************************************************
PRO GetBrowseFile, DirName, FileName, PathNum, OrbitNum, BlockBeg, $
                   BlockEnd, CamName, Retval
;***************************************************************************
; function added by D.Nelson, 12/04
; routine gets the names of 1 camera file the user wants to browse;
; file name must conform to MISR naming conventions
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1
cam_names = STRUPCASE(!KON.Instr.CAM_NAMES)

;---------------------------------------------------------------------------
; The user selects the camera to use. Either ellipsoid or terrain files will
; do. Then get the directory and file name separately.
;---------------------------------------------------------------------------

FilenameFilter = ['MISR*TERRAIN*.hdf', 'MISR*ELLIPSOID*.hdf']

GetLastFilename, 0, !KON.FileTyp.TypeBrowse, FilenameFilter, 1, $
                 file_outpath, input_filename

IF (input_filename EQ '') THEN RETURN

DirName = file_outpath
nlen = STRLEN(DirName)
FileName = STRMID(input_filename, nlen)

;---------------------------------------------------------------------------
; Determine the orbit number and camera name.
;---------------------------------------------------------------------------

npos = STRPOS(FileName, '_O')
PathNum = FIX(STRMID(FileName, npos-3, 3))
OrbitNum = LONG(STRMID(FileName, npos+2, 6))
CamName = STRMID(FileName, npos+9, 2)

;---------------------------------------------------------------------------
; Find the first and last block number from the file.
;---------------------------------------------------------------------------

GetFirstLastBlocks, input_filename, BlockBeg, BlockEnd, Retval

END ; GetBrowseFile

;***************************************************************************
PRO ShowOrbitBrowseData, CamFile, FirstBlock, LastBlock, BandChoice, $
                         CamName, Resolution, EorT, RorN, Status
;***************************************************************************
; Call this from button on main widget followed by XMANAGER call.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON orbit_image, OrbitImage_SizeX, OrbitImage_SizeY, NBLOCK_ORBIT, $
                    BLOCK_OFFSETS, ORBIT_WINDOW, DoneButton

  ;-------------------------------------------------------------------------
  ; Initialize parameters.  Resolution = 0 => 1100 m; = 1 => 275 m.
  ;-------------------------------------------------------------------------

  Status = -1

  NIR = 0
  RED = 1
  xres = LONG([!KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_CROSS])
  yres = LONG([!KON.Instr.LO_RES_PIX_ALONG, !KON.Instr.HI_RES_PIX_ALONG])
  fine_pix_size = !KON.Instr.HI_RES_PIX_SIZE * 1000.0
  ORBIT_WINDOW = -1

  nbands = 3   ;  use NIR or red plus green and blue bands

  IF (Resolution EQ 0) THEN BEGIN
     counts = [xres[0],yres[0]]
     IF (BandChoice EQ RED) THEN BEGIN
       band_ndx = [2,1,0]
       nskip = [4,1,1]
       RorN = 1
     ENDIF ELSE BEGIN
       band_ndx = [3,1,0]
       nskip = [1,1,1]
       RorN = 0
     ENDELSE
  ENDIF ELSE BEGIN
     counts = [xres[1],yres[1]]
     band_ndx = [2,1,0]
     nskip = [1,1,1]
     RorN = 1
  ENDELSE

  IF (Resolution EQ 0 AND CamName EQ 'AN') THEN BEGIN
    nskip = [4,4,4]
  ENDIF

  BLOCK_XSIZE = xres[Resolution]
  BLOCK_YSIZE = yres[Resolution]

  NBLOCK_ORBIT = LastBlock - FirstBlock + 1

  ;-------------------------------------------------------------------------
  ; Determine if this is an ellipsoid or terrain file.
  ;-------------------------------------------------------------------------

  EorT = -1
  IF (STRPOS(CamFile, 'ELLIPSOID') NE -1) THEN EorT = 0
  IF (STRPOS(CamFile, 'TERRAIN')   NE -1) THEN EorT = 1
  IF (EorT LT 0) THEN BEGIN
     ORBIT_IMAGE = 0
     SafeWDELETE, ORBIT_WINDOW, didit
     ORBIT_WINDOW = -1
     RETURN
  ENDIF

  ;-------------------------------------------------------------------------
  ; Print a message to a text widget.
  ;-------------------------------------------------------------------------

  fetching_mssg = 'Fetching data:  '+ CamFile + ' ' + $
         STRTRIM(FirstBlock,2) + '-' + $
         STRTRIM(LastBlock,2) + ' (' + $
         STRTRIM(NBLOCK_ORBIT,2) + ' blocks)'

  fetching_tlb = WIDGET_BASE( $
                        TITLE='Loading radiance data for selected camera')
  fetching_txt = WIDGET_TEXT(fetching_tlb, VALUE=fetching_mssg)
  WIDGET_CONTROL,fetching_txt, XSIZE=STRLEN(fetching_mssg), /REALIZE
  XMANAGER,'fetching', fetching_tlb, /NO_BLOCK

  ;-------------------------------------------------------------------------
  ; Show hourglass cursor. These are specific to OS type. You must have a
  ; window on screen first, else this creates one!
  ;-------------------------------------------------------------------------

;  IF (!KON.Misc.MINX_PLATFORM EQ 1) THEN DEVICE, CURSOR_STANDARD=26
;  IF (!KON.Misc.MINX_PLATFORM EQ 2) THEN DEVICE, CURSOR_STANDARD=32514

  ;-------------------------------------------------------------------------
  ; Open the HDF file, then the scientific dataset interface.
  ;-------------------------------------------------------------------------

  fileid = HDF_OPEN(CamFile, /READ)

  sd_id = HDF_StartInterface(CamFile)
  IF (sd_id LE 0) THEN RETURN
  
  ;-------------------------------------------------------------------------
  ; Get some information.
  ;-------------------------------------------------------------------------

  HDF_SD_FILEINFO, sd_id, datasets, attributes

  dim = LONARR(3,datasets)   ; Store dimensions
  fill = UINTARR(datasets)   ; Store fill 

  FOR i = 0, datasets-1 DO BEGIN
    sds_id_1=HDF_SD_SELECT(sd_id, i)
    HDF_SD_GETINFO, sds_id_1, NAME=nm, DIMS=dm, FILL=fl
    HDF_SD_ENDACCESS, sds_id_1

    dim[*,i] = dm
    fill[i] = fl
  ENDFOR

  ;-------------------------------------------------------------------------
  ; Get the block offsets for all blocks and store in array.
  ;-------------------------------------------------------------------------

  IF (Resolution EQ 1) THEN BEGIN

     vd_ref = HDF_VD_FIND(fileid, 'PerBlockMetadataCommon')
     vdata  = HDF_VD_ATTACH(fileid, vd_ref)
     nrec   = HDF_VD_READ(vdata, along_coords, $
                          FIELDS='Block_coor_ulc_som_meter.y')
     HDF_VD_DETACH, vdata

     offsets = INTARR(NBLOCK_ORBIT)

     FOR iblk = 1, NBLOCK_ORBIT-1 DO BEGIN
        offsets[iblk] = FIX((along_coords[FirstBlock-1] - $
                             along_coords[FirstBlock+iblk-1]) / $
                             fine_pix_size)
     ENDFOR

     min_offset = MIN(offsets)
     offsets -= min_offset
     off = FIX((along_coords[FirstBlock] - $
                along_coords[FirstBlock-1]) / fine_pix_size)
     IF (FirstBlock EQ LastBlock) THEN BEGIN
        offsets[0] = 0
     ENDIF ELSE BEGIN
        offsets[0] = offsets[1] + off
     ENDELSE
     max_offset = MAX(offsets)

  ENDIF

  ;-------------------------------------------------------------------------
  ; Process the 3 bands.
  ;-------------------------------------------------------------------------

  FOR iband=0,2 DO BEGIN

    ;-----------------------------------------------------------------------
    ; Import information on data and select the bands.
    ;-----------------------------------------------------------------------

    xd = dim[0,band_ndx[iband]]
    yd = dim[1,band_ndx[iband]]
    filld = fill[band_ndx[iband]]

    sds_id_1 = HDF_SD_SELECT(sd_id, band_ndx[iband])

    numx = xd / nskip[iband]
    numy = yd / nskip[iband]

    ;-----------------------------------------------------------------------
    ; Read the data.
    ;-----------------------------------------------------------------------

    HDF_SD_GETDATA, sds_id_1, hdf_image, $
                    START=[0,0,FirstBlock-1], $
                    STRIDE=[nskip[iband],nskip[iband],1], $
                    COUNT=[numx,numy,NBLOCK_ORBIT]

    HDF_SD_ENDACCESS, sds_id_1  

    ;-----------------------------------------------------------------------
    ; Eliminate large fill numbers and dropouts by masking.
    ;-----------------------------------------------------------------------

    badval = filld < 65507
    badndx = WHERE(hdf_image GE badval, numbad, /L64)
    IF (numbad GT 0) THEN hdf_image[badndx] = 0
    badndx = 0

    goodndx = WHERE(hdf_image NE 0, numgood, /L64)
    goodndx = 0
    IF (numgood EQ 0) THEN BEGIN
       mssg = 'No data are available in selected blocks. Quitting.'
       rval = DIALOG_MESSAGE(mssg, /ERROR)
       WIDGET_CONTROL,fetching_tlb,/DESTROY
       ORBIT_IMAGE = 0
       SafeWDELETE, ORBIT_WINDOW, didit
       ORBIT_WINDOW = -1
       RETURN
    ENDIF

    ;-----------------------------------------------------------------------
    ; Catch errors if user asks for too many blocks.
    ;-----------------------------------------------------------------------

    CATCH, error_status
    IF error_status NE 0 THEN BEGIN
       mssg = ['You may not have enough memory to', $
                'load this many blocks. Try again.']
       rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
       CATCH, /CANCEL
       GOTO, cleanit
    ENDIF

    ;-----------------------------------------------------------------------
    ; If intent is to display hi-res data, then assume we have only a few
    ; blocks, so it's OK to offset blocks without increasing the width of
    ; the memory array excessively.
    ;-----------------------------------------------------------------------

    IF (Resolution EQ 1) THEN BEGIN

       Xsize = LONG(numx) + offsets[NBLOCK_ORBIT-1]
       Ysize = LONG(numy) * NBLOCK_ORBIT

       ;--------------------------------------------------------------------
       ; Create the array to contain the data and the image array.
       ;--------------------------------------------------------------------

       IF (iband EQ 0) THEN BEGIN
          values = UINTARR(3, Xsize, Ysize)
          Ximgsize = Xsize
          Yimgsize = Ysize
       ENDIF

       ;--------------------------------------------------------------------
       ; Fill the data array.
       ;--------------------------------------------------------------------

       IF (numx EQ xres[0]) THEN $
          values[iband,0:xres[1]-1,*] = $
             REBIN(REFORM(hdf_image, numx, numy * NBLOCK_ORBIT), BLOCK_XSIZE, $
                   BLOCK_YSIZE * NBLOCK_ORBIT, /SAMPLE)

       IF (numx EQ xres[1]) THEN $
          values[iband,0:xres[1]-1,*] = $
             REFORM(hdf_image, BLOCK_XSIZE, BLOCK_YSIZE * NBLOCK_ORBIT)

       hdf_image = 0

       ;--------------------------------------------------------------------
       ; Now offset the blocks within the array.
       ;--------------------------------------------------------------------

       FOR iblk=0,NBLOCK_ORBIT-1 DO BEGIN
          off = max_offset - offsets[iblk]
          temp_ary = REFORM(values[iband,*,iblk*yres[1]:(iblk+1)*yres[1]-1])
          values[iband,*,iblk*yres[1]:(iblk+1)*yres[1]-1] = $
                SHIFT(temp_ary, off, 0)
          temp_ary = 0
       ENDFOR
 
    ENDIF ELSE BEGIN

       Ximgsize = numx
       Yimgsize = numy * NBLOCK_ORBIT

       ;--------------------------------------------------------------------
       ; Create the array to contain the data and the image array.
       ;--------------------------------------------------------------------

       IF (iband EQ 0) THEN values = UINTARR(3, numx, numy * NBLOCK_ORBIT)

       ;--------------------------------------------------------------------
       ; Reformat the band data and copy into a 3-band array.
       ;--------------------------------------------------------------------

       values[iband,*,*] = REFORM(hdf_image, BLOCK_XSIZE, BLOCK_YSIZE * $
                                                          NBLOCK_ORBIT)
       hdf_image = 0

    ENDELSE

    CATCH, /CANCEL

  ENDFOR

  ;-------------------------------------------------------------------------
  ; Close the HDF file.
  ;-------------------------------------------------------------------------

  HDF_SD_END, sd_id
  HDF_CLOSE, fileid

  ;-------------------------------------------------------------------------
  ; Start color scaling - handle high resolution data differently.
  ;-------------------------------------------------------------------------

  MinMax = [[!KON.Misc.LARGE_POS_NUM,  !KON.Misc.LARGE_POS_NUM, $
             !KON.Misc.LARGE_POS_NUM], $
            [!KON.Misc.LARGE_NEG_NUM,  !KON.Misc.LARGE_NEG_NUM, $
             !KON.Misc.LARGE_NEG_NUM]]

  ;----------------------------------------------------------------------
  ; If a pixel is bad in any band, make it bad in all bands.
  ;----------------------------------------------------------------------

  CATCH, error_status
  IF error_status NE 0 THEN BEGIN
     mssg = ['You may not have enough memory to', $
             'load this many blocks. Try again.']
     rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
     CATCH, /CANCEL
     GOTO, cleanit
  ENDIF

;  zero_mask = (values NE 0)
;  values = TEMPORARY(values) * (~zero_mask)
;  zero_mask = 0

  ndxbad = WHERE(values EQ 0, numbad)
  IF (numbad) THEN values[ndxbad] = 0
  ndxbad = 0

  FOR iband=0,2 DO BEGIN
     band_ary = REFORM(values[iband,*,*])
     ndxgood  = WHERE(band_ary[*,*] GT 0, numgood)
     band_ary = 0

     minval = MIN(values[ndxgood], MAX=maxval)
     mean   = MEAN(values[ndxgood])
     stddev = STDDEV(values[ndxgood])
     ndxgood = 0

     IF (minval LT 0.0) THEN minval = 0.0
     IF (maxval GT mean+stddev*4) THEN maxval = mean+stddev*4

     MinMax[iband,0] = minval < MinMax[iband,0]
     MinMax[iband,1] = maxval > MinMax[iband,1]
  ENDFOR

  MinMax[*,0] = (MEAN(MinMax[*,0]) + MinMax[*,0]) / 2
  MinMax[*,1] = (MEAN(MinMax[*,1]) + MinMax[*,1]) / 2

  ;-------------------------------------------------------------------------
  ; Create a memory pixmap and save the window ID.
  ;-------------------------------------------------------------------------

  WINDOW, XSIZE=Ximgsize, YSIZE=Yimgsize, /FREE, /PIXMAP
  ORBIT_WINDOW = !D.WINDOW
  SafeWSET, ORBIT_WINDOW, didit

  ;-------------------------------------------------------------------------
  ; Create the image for display.
  ;-------------------------------------------------------------------------

  values[0,*,*] = BYTSCL(TEMPORARY(values[0,*,*]), MIN=MinMax[0,0], $
                                                   MAX=MinMax[0,1])
  values[1,*,*] = BYTSCL(TEMPORARY(values[1,*,*]), MIN=MinMax[1,0], $
                                                   MAX=MinMax[1,1])
  values[2,*,*] = BYTSCL(TEMPORARY(values[2,*,*]), MIN=MinMax[2,0], $
                                                   MAX=MinMax[2,1])
  ORBIT_IMAGE = values
  values = 0

  CATCH, /CANCEL

  ;-------------------------------------------------------------------------
  ; Destroy the window that announces fetching data.
  ;-------------------------------------------------------------------------

  WIDGET_CONTROL, fetching_tlb, /DESTROY
      
  ;-------------------------------------------------------------------------
  ; Draw the image to the memory pixmap.
  ;-------------------------------------------------------------------------

  sizes = SIZE(ORBIT_IMAGE)
  OrbitImage_SizeX = sizes[2]
  OrbitImage_SizeY = sizes[3]

  TV, ORBIT_IMAGE, /ORDER, TRUE=1
  ORBIT_IMAGE = 0

  ;-------------------------------------------------------------------------
  ; Write block numbers on the image.
  ;-------------------------------------------------------------------------

  FOR iblk = 0, NBLOCK_ORBIT-1 DO BEGIN
     xcrd = 10
     ycrd = iblk * BLOCK_YSIZE + 0.5 * BLOCK_YSIZE
     XYOUTS, xcrd, ycrd, STRTRIM(STRING(LastBlock-iblk),2), $
             ALIGNMENT=0.0, CHARSIZE=1.0, COLOR=65535, /DEVICE
  ENDFOR

  ;-------------------------------------------------------------------------
  ; Draw 17.6 km region boundaries on image if only 1 block is displayed.
  ; 32/8 regions in a block. Also print the region number in each region.
  ; This feature removed 10/2012.
  ;-------------------------------------------------------------------------

  IF (0 AND NBLOCK_ORBIT EQ 1 AND BLOCK_XSIZE GE 2000) THEN BEGIN

     xdel = BLOCK_XSIZE / 32.0
     ydel = BLOCK_YSIZE / 8.0

     FOR irgn = 1,31 DO BEGIN
        xcrd = [xdel*irgn,xdel*irgn]
        ycrd = [0,BLOCK_YSIZE]
        PLOTS, xcrd,ycrd, /DEVICE, LINESTYLE=0, COLOR=65535, THICK=1.0
     ENDFOR

     FOR irgn = 1,7 DO BEGIN
        xcrd = [0,BLOCK_XSIZE]
        ycrd = [ydel*irgn,ydel*irgn]
        PLOTS, xcrd,ycrd, /DEVICE, LINESTYLE=0, COLOR=65535, THICK=1.0
     ENDFOR

     FOR ii = 0,31 DO BEGIN
        FOR jj = 0,7 DO BEGIN
           rgn_nums = STRTRIM(STRING(ii+1),2) + ',' + STRTRIM(STRING(jj+1),2)
           XYOUTS, xdel*ii+xdel/8, ydel*(8-jj)-ydel/8-!D.Y_CH_SIZE/2, $
                   rgn_nums, /DEVICE, COLOR=black, CHARSIZE=0.8, ALIGNMENT=0.0
        ENDFOR
     ENDFOR
  ENDIF

;  IF (!KON.Misc.MINX_PLATFORM EQ 1) THEN DEVICE, CURSOR_STANDARD=2
;  IF (!KON.Misc.MINX_PLATFORM EQ 2) THEN DEVICE, CURSOR_STANDARD=32512

  Status = 0

  RETURN

cleanit:
;  IF (!KON.Misc.MINX_PLATFORM EQ 1) THEN DEVICE, CURSOR_STANDARD=2
;  IF (!KON.Misc.MINX_PLATFORM EQ 2) THEN DEVICE, CURSOR_STANDARD=32512

  SafeWDELETE, ORBIT_WINDOW, didit
  ORBIT_WINDOW = -1

  Status = -1

END  ;  ShowOrbitBrowseData
