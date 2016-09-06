;============================================================================
;                                                                           =
;                                   MINX                                    =
;                                                                           =
;============================================================================
;                                                                           =
;                         Jet Propulsion Laboratory                         =
;                                   MISR                                    =
;                                                                           =
;        Copyright 2007-2015 by California Institute of Technology.         =
;                          ALL RIGHTS RESERVED.                             =
;                                                                           =
; U.S. Government sponsorship acknowledged. Any commercial use must be      =
; negotiated with the Office of Technology Transfer at the California       =
; Institute of Technology.                                                  =
;                                                                           =
; This software may be subject to U.S. export control laws. By accepting    = 
; this software, user agrees to comply with all applicable U.S. export laws = 
; and regulations. User has the responsibility to obtain export licenses,   = 
; or other export authority as may be required before exporting such        =
; information to foreign countries or providing access to foreign persons.  =
;                                                                           =
;============================================================================

;****************************************************************************
FUNCTION HDF_StartInterface, Filename
;****************************************************************************
; Test whether the filename is a valid file and can be successfully opened
; and read by the HDF_SD_START() function.
;----------------------------------------------------------------------------

ON_IOERROR, bad_file
sd_id = HDF_SD_START(Filename, /READ)
IF ISA(sd_id, /NUMBER) THEN RETURN, sd_id

bad_file: 
mssg = ['File: ' + Filename, 'may be the wrong type, empty or corrupted. ', $
        'Inspect the file and related files and if necessary reorder ' + $
        'the file before trying again.']
rtrn_val = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
ON_IOERROR, NULL
RETURN, -1

END  ;  HDF_StartInterface

;****************************************************************************
PRO GetFirstLastBlocks, Filename, BegBlock, EndBlock, Retval
;****************************************************************************
; Return the first and last block numbers in a MISR HDF file.
;----------------------------------------------------------------------------

Retval = -1

sd_id = HDF_StartInterface(Filename)
IF (sd_id LE 0) THEN RETURN

dindex = HDF_SD_ATTRFIND(sd_id, 'Start_block')
IF (dindex LE 0) THEN BEGIN
   mssg = ['Could not find Start_block in:', Filename]
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   HDF_SD_END, sd_id
   RETURN
ENDIF
HDF_SD_ATTRINFO, sd_id, dindex, DATA=BegBlock

dindex = HDF_SD_ATTRFIND(sd_id, 'End block')
IF (dindex LE 0) THEN BEGIN
   mssg = ['Could not find End block in:', Filename]
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   HDF_SD_END, sd_id
   RETURN
ENDIF
HDF_SD_ATTRINFO, sd_id, dindex, DATA=EndBlock

HDF_SD_END, sd_id

Retval = 0

END ; GetFirstLastBlocks

;****************************************************************************
PRO GetBlockShifts, CoordStruct, BlkPixOffset, whichorbit
;****************************************************************************
; This function retrieves the proper offset in pixels at current resolution
; for each block to be displayed.
; Creation Date: 09/16/03 by David Nelson
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;-------------------------------------------------------------------------
   ; Create an array to hold the per-block individual block offsets and the
   ; per-block cumulative block offsets.
   ;-------------------------------------------------------------------------

   pix_offset = INTARR(CoordStruct.(whichorbit).NumBlk, 2)
   pix_offset[0,*] = 0

   ;-------------------------------------------------------------------------
   ; Compute across size of a pixel in meters at this resolution.
   ;-------------------------------------------------------------------------

   pix_size = ((*CoordStruct.(whichorbit).LRCcrossSOM)[0] - $ 
               (*CoordStruct.(whichorbit).ULCcrossSOM)[0]) / $
               CoordStruct.(whichorbit).DimCross

   ;-------------------------------------------------------------------------
   ; Compute the block offsets from the previous block to this one using the
   ; SOM upper left corner coordinate data.
   ;-------------------------------------------------------------------------

   FOR iblk = 1, CoordStruct.(whichorbit).NumBlk-1 DO BEGIN
      blk_offset = (*CoordStruct.(whichorbit).ULCcrossSOM)[iblk] - $
                   (*CoordStruct.(whichorbit).ULCcrossSOM)[iblk-1]
      pix_offset[iblk,0] = ROUND(blk_offset / pix_size)
      pix_offset[iblk,1] = pix_offset[iblk-1,1] + pix_offset[iblk,0]
   ENDFOR

   BlkPixOffset = PTR_NEW(pix_offset, /NO_COPY)

END  ;  GetBlockShifts

;****************************************************************************
PRO SetMINXCursor, Selection, Retval
;****************************************************************************
; Set the fine crosshair cursor to be used in the draw window.
;----------------------------------------------------------------------------

Retval =-1

;----------------------------------------------------------------------------
; Define and set the cursor.
;----------------------------------------------------------------------------

IF (Selection EQ 0) THEN BEGIN
   IF (!KON.Misc.MINX_platform EQ 1) THEN DEVICE, CURSOR_STANDARD=2
   IF (!KON.Misc.MINX_platform EQ 2) THEN DEVICE, CURSOR_STANDARD=32512
ENDIF

IF (Selection EQ 1) THEN BEGIN
   DIGITZ_CURSOR = $
         FIX([    0, 32639,   320,   320,   320,   320,   320, 16705, $
                  0, 16705,   320,   320,   320,   320,   320, 32639], TYPE=12)
   DIGITZ_MASK = $
         FIX([    0, 65087, 33088, 33088, 33088, 33088, 33088, 16705, $
              16254, 16705, 33088, 33088, 33088, 33088, 33088, 65087], TYPE=12)
   DIGITZ_CENTER = [7, 6]

   DEVICE, CURSOR_IMAGE=DIGITZ_CURSOR, CURSOR_XY=DIGITZ_CENTER, $
           CURSOR_MASK=DIGITZ_MASK
ENDIF

IF (Selection EQ 2) THEN BEGIN
   DELETE_CURSOR = $
         FIX([    0,   320,   544,  1360,  2600,  5140, 10250, 20485, $
               8194, 20485, 10250,  5140,  2600,  1360,   544,   320], TYPE=12)
   DELETE_MASK   = $
         FIX([    0,   320,   864,  1904,  3640,  7196, 14350, 28679, $
              24578, 28679, 14350,  7196,  3640,  1904,   864,   320], TYPE=12)
   DELETE_CENTER = [7, 7]

   DEVICE, CURSOR_IMAGE=DELETE_CURSOR, CURSOR_XY=DELETE_CENTER, $
           CURSOR_MASK=DELETE_MASK
ENDIF

Retval = 0

END  ;  SetMINXCursor

;****************************************************************************
FUNCTION GetScaleFactorMisr, filename, BandNdx, whichorbit
;****************************************************************************
; Get the scale factors and parameters to correctly convert DN to radiances
; and reflectances. This function was provided by Mike G after a bug was
; found in the old version. The old version persists largely in the function
; GetScaleFactorAMisr for AirMISR.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

grid_id = EOS_GD_OPEN(filename)   ; Open the HDF EOS Grid Interface

;----------------------------------------------------------------------------
; Read the blue data
;----------------------------------------------------------------------------

IF (BandNdx EQ 0) THEN BEGIN
   gr_id = EOS_GD_ATTACH(grid_id,'BlueBand')
   status = EOS_GD_READATTR(gr_id,'Scale factor',dum)
   scl_fctr = dum[0]     ; First value is scale factor
   status = EOS_GD_READATTR(gr_id,'std_solar_wgted_height',dum)
   sol_irrd = dum[0]     ; First value is solar irradiance
   status = EOS_GD_READATTR(gr_id,'SunDistanceAU',dum)
   es_dist = dum[0]      ; First value is Earth-Sun distance (AU)
   status = EOS_GD_DETACH(gr_id)
ENDIF

;----------------------------------------------------------------------------
; Read the green data
;----------------------------------------------------------------------------

IF (BandNdx EQ 1) THEN BEGIN
   gr_id = EOS_GD_ATTACH(grid_id,'GreenBand')
   status = EOS_GD_READATTR(gr_id,'Scale factor',dum)
   scl_fctr = dum[0]     ; First value is scale factor
   status = EOS_GD_READATTR(gr_id,'std_solar_wgted_height',dum)
   sol_irrd = dum[0]     ; First value is solar irradiance
   status = EOS_GD_READATTR(gr_id,'SunDistanceAU',dum)
   es_dist = dum[0]      ; First value is Earth-Sun distance (AU)
   status = EOS_GD_DETACH(gr_id)
ENDIF

;----------------------------------------------------------------------------
; Read the red data
;----------------------------------------------------------------------------

IF (BandNdx EQ 2) THEN BEGIN
   gr_id = EOS_GD_ATTACH(grid_id,'RedBand')
   status = EOS_GD_READATTR(gr_id,'Scale factor',dum)
   scl_fctr = dum[0]     ; First value is scale factor
   status = EOS_GD_READATTR(gr_id,'std_solar_wgted_height',dum)
   sol_irrd = dum[0]     ; First value is solar irradiance
   status = EOS_GD_READATTR(gr_id,'SunDistanceAU',dum)
   es_dist = dum[0]      ; First value is Earth-Sun distance (AU)
   status = EOS_GD_DETACH(gr_id)
ENDIF

;----------------------------------------------------------------------------
; Read the nir data
;----------------------------------------------------------------------------

IF (BandNdx EQ 3) THEN BEGIN
   gr_id = EOS_GD_ATTACH(grid_id,'NIRBand')
   status = EOS_GD_READATTR(gr_id,'Scale factor',dum)
   scl_fctr = dum[0]     ; First value is scale factor
   status = EOS_GD_READATTR(gr_id,'std_solar_wgted_height',dum)
   sol_irrd = dum[0]     ; First value is solar irradiance
   status = EOS_GD_READATTR(gr_id,'SunDistanceAU',dum)
   es_dist = dum[0]      ; First value is Earth-Sun distance (AU)
   status = EOS_GD_DETACH(gr_id)
ENDIF

;----------------------------------------------------------------------------
; Close the HDF EOS Grid Interface and save the result
;----------------------------------------------------------------------------

status = EOS_GD_CLOSE(grid_id)

RETURN, scl_fctr

END  ;  GetScaleFactorMisr

;****************************************************************************
FUNCTION GetScaleFactorAMisr, filename, BandNdx, whichorbit
;****************************************************************************
; This function returns the DN to Radiance "Scale factor" for the requested
; band index for 1 camera. Originally written as misr_get_meta_info.pro for
; misr_view by C Thompson.  Converted from misr_get_meta_info.pro to
; GetScaleFactor by J Hall 3/31/04
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  bandname1 = ''
  bandname2 = 'junkjunk'

  CASE 1 OF
     BandNdx EQ 0 : bandname1 = 'Blue Radiance'
     BandNdx EQ 1 : bandname1 = 'Green Radiance'
     BandNdx EQ 2 : bandname1 = 'Red Radiance'
     BandNdx EQ 3 : BEGIN
        bandname1 = 'Infrared Radiance/RDQI'
        bandname2 = 'NIR Radiance/RDQI'
     END
  ENDCASE

  name = 'Rad_scale_factor (1=Blue;2=Green;3=Red;4=Nir)'
  get_data = 1

  ;--------------------------------------------------------------------------
  ; Set up tag value to search upon.
  ;--------------------------------------------------------------------------

  VDATA_TAG = 1962

  ;--------------------------------------------------------------------------
  ; Open up file as vanilla HDF file.
  ;--------------------------------------------------------------------------

  fileID = HDF_OPEN( fileName, /READ )

  ;--------------------------------------------------------------------------
  ; obtain number of VDATA sets
  ;--------------------------------------------------------------------------

  nVdatas = HDF_NUMBER( fileID, TAG = VDATA_TAG )

  vRef = (-1)
  vdata_ctr = 0L
  done = 0

  ;--------------------------------------------------------------------------
  ; Go through WHILE loop until the requested VDATA is located.
  ;--------------------------------------------------------------------------

  WHILE (~ done AND vdata_ctr LT nVdatas) DO BEGIN
    vRef = HDF_VD_GETID( fileID, vRef )
    vdataID = HDF_VD_ATTACH( fileID, vRef, /READ )
    HDF_VD_GET, vdataID, NAME = nm

    ;------------------------------------------------------------------------
    ; NOT FINISHED !
    ;------------------------------------------------------------------------

    IF get_data THEN BEGIN
      IF nm EQ name THEN BEGIN
        res = HDF_VD_READ( vdataID, Scale_factor )
        done = 1
      ENDIF
    ENDIF

    vdata_ctr = vdata_ctr + 1
    HDF_VD_DETACH, vdataID
  ENDWHILE

  ;--------------------------------------------------------------------------
  ; Close HDF file.
  ;--------------------------------------------------------------------------

  HDF_CLOSE, fileID

  RETURN, Scale_factor

END ; GetScaleFactorAMisr

;****************************************************************************
FUNCTION GetSunDistance, filename
;****************************************************************************
; This function returns the Sun_distance 1 camera. Originally written as
; misr_get_meta_info.pro for misr_view by C Thompson.  Converted from
; GetScaleFactor to GetSunDistance by J Hall 4/21/04.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  name = 'Sun_distance'

  ;--------------------------------------------------------------------------
  ; Set up tag value to search upon.
  ;--------------------------------------------------------------------------

  VDATA_TAG = 1962

  ;--------------------------------------------------------------------------
  ; Open up file as vanilla HDF file.
  ;--------------------------------------------------------------------------

   fileID = HDF_OPEN( fileName, /READ )

  ;--------------------------------------------------------------------------
  ; Obtain number of VDATA sets.
  ;--------------------------------------------------------------------------

  nVdatas = HDF_NUMBER( fileID, TAG = VDATA_TAG )

  vRef = (-1)
  vdata_ctr = 0L
  done = 0

  ;--------------------------------------------------------------------------
  ; Go through loop until requested VDATA is located.
  ;--------------------------------------------------------------------------

  WHILE (~ done AND vdata_ctr LT nVdatas) DO BEGIN
    vRef = HDF_VD_GETID( fileID, vRef )
    vdataID = HDF_VD_ATTACH( fileID, vRef, /READ )
    HDF_VD_GET, vdataID, NAME = nm

    IF nm EQ name THEN BEGIN
      res = HDF_VD_READ( vdataID, Sun_distance )
      done = 1
    ENDIF

    vdata_ctr = vdata_ctr + 1
    HDF_VD_DETACH, vdataID
  ENDWHILE

  ;--------------------------------------------------------------------------
  ; Close HDF file.
  ;--------------------------------------------------------------------------

  HDF_CLOSE, fileID

  RETURN, Sun_distance

END ; GetSunDistance

;****************************************************************************
FUNCTION GetStdSolarWgtedHeight, filename, BandNdx, whichorbit
;****************************************************************************
; This function returns AirMISR std_solar_wgted_height for the requested band
; index for one camera. Converted from GetScaleFactor to GetStdSolarWgtedHeight
; by J Hall 4/21/04.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  name = 'std_solar_wgted_height (1=Blue;2=Green;3=Red;4=Nir)'

  ;--------------------------------------------------------------------------
  ; Set up tag value to search upon.
  ;--------------------------------------------------------------------------

  VDATA_TAG = 1962

  ;---------------------------------------------------------------------------
  ; Open up file as vanilla HDF file.
  ;---------------------------------------------------------------------------

   fileID = HDF_OPEN( fileName, /READ )

  ;--------------------------------------------------------------------------
  ; Obtain number of VDATA sets.
  ;--------------------------------------------------------------------------

  nVdatas = HDF_NUMBER( fileID, TAG = VDATA_TAG )

  vRef = (-1)
  vdata_ctr = 0L
  done = 0

  ;--------------------------------------------------------------------------
  ; Go through loop until requested VDATA is located.
  ;--------------------------------------------------------------------------

  WHILE (~ done AND vdata_ctr LT nVdatas) DO BEGIN
    vRef = HDF_VD_GETID( fileID, vRef )
    vdataID = HDF_VD_ATTACH( fileID, vRef, /READ )
    HDF_VD_GET, vdataID, NAME = nm

    IF nm EQ name THEN BEGIN
      res = HDF_VD_READ( vdataID, Std_solar_wgted_height )
      done = 1
    ENDIF

    vdata_ctr = vdata_ctr + 1
    HDF_VD_DETACH, vdataID
  ENDWHILE

  ;--------------------------------------------------------------------------
  ; Close HDF file.
  ;--------------------------------------------------------------------------

  HDF_CLOSE, fileID

  RETURN, Std_solar_wgted_height[BandNdx]

END ; GetStdSolarWgtedHeight

;****************************************************************************
FUNCTION GetBrfConversionFactors, filename, BandNdx, NumBlk, BlkBeg, $
                                  AIRMISR=AirMISR, whichorbit
;****************************************************************************
; This function returns the datablock of BRF conversion factors for requested
; bandname of all requested blocks for 1 camera. Converted from FullAnI to
; GetBrfConversionFactors on 3/31/04 by J Hall.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  ;--------------------------------------------------------------------------
  ; Advance band index to BRF conversion factor index.
  ;--------------------------------------------------------------------------

  convfact_ndx = BandNdx + 4

  ;--------------------------------------------------------------------------
  ; Open the HDF file, then the scientific dataset interface.
  ;--------------------------------------------------------------------------

  sd_id = HDF_StartInterface(filename)
  IF (sd_id LE 0) THEN RETURN, -1

  ;--------------------------------------------------------------------------
  ; Get some information.
  ;--------------------------------------------------------------------------

  HDF_SD_FILEINFO, sd_id, datasets, attributes

  CASE 1 OF                  ; Store dimensions
    AirMISR EQ 0 : dim = LONARR(3,datasets)  ;MISR
    AirMISR EQ 1 : dim = LONARR(2,datasets)  ;AirMISR
  ENDCASE

  FOR i = 0, datasets-1 DO BEGIN
    sds_id_1=HDF_SD_SELECT(sd_id, i)
    HDF_SD_GETINFO, sds_id_1, DIMS=dm
    HDF_SD_ENDACCESS, sds_id_1

    dim[*,i] = dm
  ENDFOR

  IF (convfact_ndx GT N_ELEMENTS(dim[0,*])) THEN BEGIN
    RETURN, -1
  ENDIF

  ;--------------------------------------------------------------------------
  ; Import information on data.
  ;--------------------------------------------------------------------------

  IF (convfact_ndx GE (SIZE(dim))[2]) THEN RETURN, -1

  xd = dim[0,convfact_ndx]
  yd = dim[1,convfact_ndx]

  ;--------------------------------------------------------------------------
  ; Select the requested channel.
  ;--------------------------------------------------------------------------

  sds_id_1 = HDF_SD_SELECT(sd_id, convfact_ndx)

  ;--------------------------------------------------------------------------
  ; Read the data.
  ;--------------------------------------------------------------------------

  CASE 1 OF

    AirMISR EQ 0 : HDF_SD_GETDATA, sds_id_1, datablock, COUNT=[xd,yd,NumBlk], $
                                   START=[0,0,BlkBeg-1]

    AirMISR EQ 1 : HDF_SD_GETDATA, sds_id_1, datablock, COUNT=[xd,yd], $
                                   START=[0,0]

   ENDCASE

  ;--------------------------------------------------------------------------
  ; Where values are BADVALUE (-444), set them to 0.
  ;--------------------------------------------------------------------------

  ndxbad = WHERE(datablock LT 0.0, numbad)
  IF (numbad GT 0) THEN datablock[ndxbad] = 0.0
  ndxbad = 0

  ;--------------------------------------------------------------------------
  ; End data access and close the HDF file.
  ;--------------------------------------------------------------------------

  HDF_SD_ENDACCESS, sds_id_1
  HDF_SD_END, sd_id

  RETURN, datablock

END ; GetBrfConversionFactors

;****************************************************************************
PRO get_AirMISR_data, CamFiles, CoordStruct, Retval, whichorbit
;****************************************************************************
; This program reads data for 9 AirMISR cameras and trims the image for each
; camera down to the extent of Nadir.
; Creation Date: 11/25/03 by Michael J. Garay
; Rewritten: 2/12/04 by David Nelson
; Converted GetMISRdata to get_AirMISR_data 4/6/04 by J Hall
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON chan_minmax, ChanMinMax

  Retval = -1
  ncam = 9               ; use all 9 cameras
  ncam_plus = ncam + 1   ; add a fake camera for a work area

  ;--------------------------------------------------------------------------
  ; Take band_name from the first orbit, since both orbits should be the same
  ; band, or band combinations.
  ;--------------------------------------------------------------------------

  whichorbit = 0

  CASE 1 OF
    CoordStruct.(whichorbit).band_ndx[0] EQ 0 : band_name = 'Terrain Blue'
    CoordStruct.(whichorbit).band_ndx[0] EQ 1 : band_name = 'Terrain Green'
    CoordStruct.(whichorbit).band_ndx[0] EQ 2 : band_name = 'Terrain Red'
    CoordStruct.(whichorbit).band_ndx[0] EQ 3 : band_name = 'Terrain Infrared'
  ENDCASE

  ;--------------------------------------------------------------------------
  ; Get the Nadir camera to determine the extent of the image data within the
  ; field of fill values.
  ;--------------------------------------------------------------------------

  done = 0

  startcam = (ncam EQ 18) ? 9 : 0

  FOR cam = startcam, (startcam+4) DO BEGIN  ; Step up to Nadir camera then stop.

    whichorbit = (cam GE 9)

    ;------------------------------------------------------------------------
    ; Open the HDF file, then the scientific dataset interface.
    ;------------------------------------------------------------------------

    fileid = HDF_OPEN(CamFiles[cam],/READ)
    sd_id = HDF_StartInterface(CamFiles[cam])
    IF (sd_id LE 0) THEN RETURN

    ;------------------------------------------------------------------------
    ; Get some information.
    ;------------------------------------------------------------------------

    HDF_SD_FILEINFO, sd_id, datasets, attributes

    dim = LONARR(2,datasets)   ; AirMISR, not MISR

    FOR i = 0, datasets-1 DO BEGIN
      sds_id_1=HDF_SD_SELECT(sd_id, i)
      HDF_SD_GETINFO, sds_id_1, DIMS=dm, NAME=nm
      HDF_SD_ENDACCESS, sds_id_1

      ;----------------------------------------------------------------------
      ; Adjust the band index from 0-3 (blue-nir) to actual index pointing to
      ; where the names match.
      ;----------------------------------------------------------------------

      IF nm EQ band_name THEN band_ndx_actual = i

      dim[*,i] = dm
    ENDFOR

    ;------------------------------------------------------------------------
    ; Import information on data.
    ;------------------------------------------------------------------------

    xd = dim[0,band_ndx_actual]
    yd = dim[1,band_ndx_actual]

    ;------------------------------------------------------------------------
    ; Select the red channel.
    ;------------------------------------------------------------------------

    sds_id_1 = HDF_SD_SELECT(sd_id, band_ndx_actual)

    IF (cam EQ 4) THEN BEGIN

      ;----------------------------------------------------------------------
      ; Read the Nadir camera to get the extent of the data within the field
      ; of background fill values.
      ;----------------------------------------------------------------------

      HDF_SD_GETDATA, sds_id_1, datablock, COUNT=[xd,yd], START=[0,0]

      x = 0
      again = 1
      WHILE again DO BEGIN
        IF MIN(datablock[x,*]) LT 65511 THEN again = 0
        x = x + 1
      ENDWHILE
      nadir_startx = x

      x = xd-1
      again = 1
      WHILE again DO BEGIN
        IF MIN(datablock[x,*]) LT 65511 THEN again = 0
        x = x - 1
      ENDWHILE
      nadir_endx = x

      y = 0
      again = 1
      WHILE again DO BEGIN
        IF MIN(datablock[*,y]) LT 65511 THEN again = 0
        y = y + 1
      ENDWHILE
      nadir_starty = y

      y = yd-1
      again = 1
      WHILE again DO BEGIN
        IF MIN(datablock[*,y]) LT 65511 THEN again = 0
        y = y - 1
      ENDWHILE
      nadir_endy = y

      IF (nadir_startx  GT CoordStruct.(whichorbit).add_edge) THEN $
         nadir_startx = nadir_startx - CoordStruct.(whichorbit).add_edge
      IF (xd-nadir_endx GT CoordStruct.(whichorbit).add_edge) THEN $
         nadir_endx   = nadir_endx   + CoordStruct.(whichorbit).add_edge
      IF (nadir_starty  GT CoordStruct.(whichorbit).add_edge) THEN $
         nadir_starty = nadir_starty - CoordStruct.(whichorbit).add_edge
      IF (yd-nadir_endy GT CoordStruct.(whichorbit).add_edge) THEN $
         nadir_endy   = nadir_endy   + CoordStruct.(whichorbit).add_edge

      xs_nadir = nadir_endx - nadir_startx + 1
      ys_nadir = nadir_endy - nadir_starty + 1

      ChanMinMax = FLTARR(4, !KON.Instr.NBAND, !KON.Instr.NCAM)

      WorkImage = FLTARR(xs_nadir, ys_nadir, 3)
      WORK_MinMax = FLTARR(3,2)	; (rgb,minmax)

    ENDIF

    ;------------------------------------------------------------------------
    ; End data access and close the HDF file.
    ;------------------------------------------------------------------------

    HDF_SD_ENDACCESS, sds_id_1
    HDF_SD_END, sd_id
    HDF_CLOSE, fileid

  ENDFOR  ;  End camera loop (to find extents of Nadir data.)

  ;--------------------------------------------------------------------------
  ; Loop through 9 cameras and read data from each one.
  ;--------------------------------------------------------------------------

  startcam = (ncam EQ 18) ? 9 : 0

  FOR cam = startcam, (startcam+8) DO BEGIN

    whichorbit = (cam GE 9)

    ;------------------------------------------------------------------------
    ; Open the HDF file, then the scientific dataset interface.
    ;------------------------------------------------------------------------

    fileid = HDF_OPEN(CamFiles[cam],/READ)
    sd_id = HDF_StartInterface(CamFiles[cam])
    IF (sd_id LE 0) THEN RETURN

    ;------------------------------------------------------------------------
    ; Get some information.
    ;------------------------------------------------------------------------

    HDF_SD_FILEINFO, sd_id, datasets, attributes

    names = STRARR(datasets)   ; Store names
    dim = LONARR(2,datasets)   ; AirMISR, not MISR
    fill  = UINTARR(datasets)  ; Store fill

    FOR i = 0, datasets-1 DO BEGIN
      sds_id_1=HDF_SD_SELECT(sd_id, i)
      HDF_SD_GETINFO, sds_id_1, NAME=nm, DIMS=dm, FILL=fl
      HDF_SD_ENDACCESS, sds_id_1

      ;----------------------------------------------------------------------
      ; Adjust the band index from 0-3 (blue-nir) to actual index pointing to
      ; where the names match.
      ;----------------------------------------------------------------------

      IF nm EQ band_name THEN band_ndx_actual = i

      ;----------------------------------------------------------------------
      ; Also remember the index of the Solar Zenith angle data.
      ;----------------------------------------------------------------------
      IF nm EQ 'Sun Zenith (degrees)' THEN sunz_ndx = i

      names[i] = nm
      dim[*,i] = dm
      fill[i] = fl
    ENDFOR

    ;------------------------------------------------------------------------
    ; Import information on data.
    ;------------------------------------------------------------------------

    xd = dim[0,band_ndx_actual]
    yd = dim[1,band_ndx_actual]
    filld = fill[band_ndx_actual]

    CoordStruct.(whichorbit).DataField = names[band_ndx_actual]
    CoordStruct.(whichorbit).DimCross  = xd
    CoordStruct.(whichorbit).DimAlong  = yd

    ;------------------------------------------------------------------------
    ; Select the red channel and read the data.
    ;------------------------------------------------------------------------

    sds_id_1 = HDF_SD_SELECT(sd_id, band_ndx_actual)
    HDF_SD_GETDATA, sds_id_1, datablock, COUNT=[xd,yd], START=[0,0]
    HDF_SD_ENDACCESS, sds_id_1

    ;------------------------------------------------------------------------
    ; Select the Solar Zenith angle and read the data.
    ;------------------------------------------------------------------------

    sds_id_2 = HDF_SD_SELECT(sd_id, sunz_ndx)
    HDF_SD_GETDATA, sds_id_2, sunzblock, COUNT=[xd,yd], START=[0,0]
    HDF_SD_ENDACCESS, sds_id_2

    dn = datablock[nadir_startx:nadir_endx,nadir_starty:nadir_endy]
    sunz = sunzblock[nadir_startx:nadir_endx,nadir_starty:nadir_endy]

    ;------------------------------------------------------------------------
    ; Close the HDF file.
    ;------------------------------------------------------------------------

    HDF_SD_END, sd_id
    HDF_CLOSE, fileid

    ;------------------------------------------------------------------------
    ; Get the factor for conversion to Radiance.
    ;------------------------------------------------------------------------

    Scale_factor = GetScaleFactorAMisr( CamFiles[cam], $
                                        CoordStruct.(whichorbit).band_ndx[0], $
                                        whichorbit )

    ;------------------------------------------------------------------------
    ; Get the Sun Distance.
    ;------------------------------------------------------------------------

    Sun_distance = GetSunDistance( CamFiles[cam] )

    ;------------------------------------------------------------------------
    ; Get std_solar_wgted_height for conversion to BRF.
    ;------------------------------------------------------------------------

    Std_solar_wgted_height = GetStdSolarWgtedHeight( CamFiles[cam], $
                                   CoordStruct.(whichorbit).band_ndx[0], $
                                   whichorbit )

    ;------------------------------------------------------------------------
    ; Eliminate large fill numbers and dropouts by masking to 0.
    ;------------------------------------------------------------------------

    MASK1 = (dn LT 65511)
    dn = TEMPORARY(dn) * MASK1
    MASK1 = 1

    MASK1 = (sunz GE 0.0)
    sunz = TEMPORARY(sunz) * MASK1
    MASK1 = 1

    ;------------------------------------------------------------------------
    ; Convert DN to radiance.
    ;------------------------------------------------------------------------

    radiance = dn * Scale_factor

    ;------------------------------------------------------------------------
    ; Convert Radiance to BRF.
    ;------------------------------------------------------------------------

    brf = FLOAT( !PI * radiance * Sun_distance / $
                 (Std_solar_wgted_height * COS(sunz/!RADEG)))

    ;------------------------------------------------------------------------
    ; Apply image to correct camera.
    ;------------------------------------------------------------------------

    !VAR.RawImages[*,*,cam] = brf

  ENDFOR  ;  End camera loop

  ;--------------------------------------------------------------------------
  ; Set all values outside the nadir data area to null so when we wrap the
  ; image during offset scrolling, we wrap nulls.
  ;--------------------------------------------------------------------------

  !VAR.RawImages[0:CoordStruct.(whichorbit).add_edge/2,*,*] = 0.0
  !VAR.RawImages[xs_nadir-CoordStruct.(whichorbit).add_edge/2:xs_nadir-1,*,*] = 0.0
  !VAR.RawImages[*,0:CoordStruct.(whichorbit).add_edge/2,*] = 0.0
  !VAR.RawImages[*,ys_nadir-CoordStruct.(whichorbit).add_edge/2:ys_nadir-1,*] = 0.0

  ;--------------------------------------------------------------------------
  ; Free memory.
  ;--------------------------------------------------------------------------

  dn = 0
  image = 0
  datablock = 0
  sunz = 0
  brf = 0

  ;--------------------------------------------------------------------------
  ; Set some variables.
  ;--------------------------------------------------------------------------

  CoordStruct.(whichorbit).ULCwndwX = 0
  CoordStruct.(whichorbit).ULCwndwY = ys_nadir
  CoordStruct.(whichorbit).LRCwndwX = xs_nadir
  CoordStruct.(whichorbit).LRCwndwY = 0

  Retval = 0

END ; get_AirMISR_data

;****************************************************************************
PRO GetMISRdata, NumCam, CamFiles, CoordStruct, Retval
;****************************************************************************
; This program gets data for 9  MISR cameras and converts data values to BRF.
; Creation Date: 11/25/03 by Michael J. Garay.
; Rewritten: 2/12/04 by David Nelson.
; Separated MISR data reading code from FullAnI into this subroutine and added
; BRF conversion: 4/6/04 by J Hall.
;----------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  Retval = -1

  num_hires_channel = 0
  ncam_plus = NumCam + 1   ; add a fake camera for a work area

  ;--------------------------------------------------------------------------
  ; Create the array to contain the raw image data.
  ;--------------------------------------------------------------------------

  !VAR.RawImages = PTRARR(CoordStruct.(0).num_band, NumCam)

  ;--------------------------------------------------------------------------
  ; Loop through the 9 or 18 cameras.
  ;--------------------------------------------------------------------------

  FOR icam = 0, NumCam-1 DO BEGIN

    whichorbit = (icam GE 9)

    ;------------------------------------------------------------------------
    ; Open the HDF file, then the scientific dataset interface.
    ;------------------------------------------------------------------------

    rval = FILE_TEST(CamFiles[icam], /NOEXPAND_PATH, /READ, /REGULAR)
    IF (rval NE 1) THEN BEGIN
      IF (icam NE 0) THEN WIDGET_CONTROL,fetching_tlb,/DESTROY
       mssg = ['This file is missing:', CamFiles[icam], $
               'Fix the problem and try running again.']
       rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
       RETURN
    ENDIF

    sd_id = HDF_StartInterface(CamFiles[icam])
    IF (sd_id LE 0) THEN BEGIN
      IF (icam NE 0) THEN WIDGET_CONTROL,fetching_tlb,/DESTROY
      RETURN
    ENDIF

    ;-------------------------------------------------------------------------
    ; Print a message to the terminal, and also to a text widget.
    ;-------------------------------------------------------------------------
    
    fetching_mssg = 'Fetching data:  ' + CamFiles[icam] + ' ' + $
       STRTRIM(CoordStruct.(whichorbit).BlkBeg, 2) + '-' + $
       STRTRIM(CoordStruct.(whichorbit).BlkEnd, 2) + ' (' + $
       STRTRIM(CoordStruct.(whichorbit).NumBlk, 2) + ' blocks)'
       
    IF (icam EQ 0) THEN BEGIN
       fetching_tlb = WIDGET_BASE(TITLE='Loading radiance data for 9 cameras')
       fetching_txt = WIDGET_TEXT(fetching_tlb, VALUE=fetching_mssg)
       WIDGET_CONTROL,fetching_txt, XSIZE=STRLEN(fetching_mssg), /REALIZE
       XMANAGER, 'fetching', fetching_tlb, /NO_BLOCK
    ENDIF ELSE BEGIN
       WIDGET_CONTROL, fetching_txt, SET_VALUE=fetching_mssg
    ENDELSE
    
    ;------------------------------------------------------------------------
    ; Get some information.
    ;------------------------------------------------------------------------

    HDF_SD_FILEINFO, sd_id, datasets, attributes

    names = STRARR(datasets)   ; Store names
    dim   = LONARR(3,datasets) ; MISR, not AirMISR
    fill  = UINTARR(datasets)  ; Store fill

    FOR i = 0, datasets-1 DO BEGIN
      sds_id_1=HDF_SD_SELECT(sd_id, i)
      HDF_SD_GETINFO, sds_id_1, NAME=nm, DIMS=dm, FILL=fl
      HDF_SD_ENDACCESS, sds_id_1

      names[i] = nm
      dim[*,i] = dm
      fill[i] = fl
    ENDFOR

    ;------------------------------------------------------------------------
    ; Accumulate the number of channels in orbit that are high resolution
    ; (2048 pixel wide swath).
    ;------------------------------------------------------------------------

    gl_ndxs = WHERE(dim[0,0:3] EQ !KON.Instr.HI_RES_PIX_CROSS, num_gl_ndxs)
    num_hires_channel += num_gl_ndxs
    gl_ndxs = 0

    ;------------------------------------------------------------------------
    ; Loop over bands and load the data.
    ;------------------------------------------------------------------------

    FOR iband = 0, CoordStruct.(whichorbit).num_band-1 DO BEGIN

       ;---------------------------------------------------------------------
       ; Import information on data.
       ;---------------------------------------------------------------------

       xd = dim[0,CoordStruct.(whichorbit).band_ndx[iband]]
       yd = dim[1,CoordStruct.(whichorbit).band_ndx[iband]]
       filld = fill[CoordStruct.(whichorbit).band_ndx[iband]]

       CoordStruct.(whichorbit).DataField = $
            names[CoordStruct.(whichorbit).band_ndx[iband]]

       ;---------------------------------------------------------------------
       ; Select the desired band.
       ;---------------------------------------------------------------------

       sds_id_1 = HDF_SD_SELECT(sd_id, CoordStruct.(whichorbit).band_ndx[iband])

       ;---------------------------------------------------------------------
       ; Read the data.
       ;---------------------------------------------------------------------

       HDF_SD_GETDATA, sds_id_1, datablock, $
                       COUNT=[xd,yd,CoordStruct.(whichorbit).NumBlk], $
                       START=[0,0,CoordStruct.(whichorbit).BlkBeg-1]

       ;---------------------------------------------------------------------
       ; End data access to this band.
       ;---------------------------------------------------------------------

       HDF_SD_ENDACCESS, sds_id_1

       ;---------------------------------------------------------------------
       ; Temporarily set DimCross and DimAlong to the input data size. These
       ; will get reset to the Nadir values a little later on.
       ;---------------------------------------------------------------------

       sizes = SIZE(datablock)
       xs = sizes[1]
       ys = sizes[2]

       CoordStruct.(whichorbit).DimCross = xs
       CoordStruct.(whichorbit).DimAlong = ys

       IF (icam EQ 4 OR icam EQ 13) THEN BEGIN
         nadir_xs = xs
         nadir_ys = ys
       ENDIF

       ;---------------------------------------------------------------------
       ; Get the factor for conversion to Radiance.
       ;---------------------------------------------------------------------

       Scale_factor = GetScaleFactorMisr(CamFiles[icam], $
                                 CoordStruct.(whichorbit).band_ndx[iband], $
                                 whichorbit)

       ;---------------------------------------------------------------------
       ; Get the factors for conversion to BRF. If data are old it might not
       ; contain BRF Conversion Factors. In that case create a 32x8 array
       ; containing values of 1.0.
       ;---------------------------------------------------------------------

       BrfConversionFactors = GetBrfConversionFactors( CamFiles[icam], $
                                    CoordStruct.(whichorbit).band_ndx[iband], $
                                    CoordStruct.(whichorbit).NumBlk, $
                                    CoordStruct.(whichorbit).BlkBeg, $
                                    AirMISR=0, whichorbit )

       IF ( N_ELEMENTS(BrfConversionFactors) EQ 1 ) THEN BEGIN

         BrfConversionFactors = $
           FLTARR(32,8,CoordStruct.(whichorbit).NumBlk)+1.0

         IF (icam EQ 0) THEN BEGIN
           mssg = ['BRF Conversion Factors were not found.', $
                   'You may be using an older data version.', $
                   'This orbit cannot be used.']
           rtrn = DIALOG_MESSAGE(mssg,/INFORMATION, /CENTER)
           Retval = -2
           RETURN
         ENDIF

       ENDIF

       ;---------------------------------------------------------------------
       ; Create padding array (the amount of block offset or shift). Pad size
       ; is 1/32 of block width (17.6 km). Initialize with -1 so we can detect
       ; the borders later.
       ;---------------------------------------------------------------------

       GetBlockShifts, CoordStruct, blk_pix_offset, whichorbit

       IF (iband EQ 0) THEN $
          CoordStruct.(whichorbit).BlkPixOffset = blk_pix_offset

       pad_size = xd / 32

       dummy = FLTARR(pad_size,yd)

       buff  = MIN((*blk_pix_offset)[*,1])
       range = MAX((*blk_pix_offset)[*,1]) - buff - pad_size
       buff  = (buff LT 0) ? buff : 0
       (*blk_pix_offset)[*,1] = (*blk_pix_offset)[*,1] - buff
       buff = 0

       ;---------------------------------------------------------------------
       ; Fill padding array - pad size is 1/32 of block width (17.6 km).
       ;---------------------------------------------------------------------

       FOR iblk = 0, CoordStruct.(whichorbit).NumBlk-1 DO BEGIN

         dum = REFORM(datablock[*,*,iblk])

         ;-------------------------------------------------------------------
         ; Convert raw data values to DNs - get rid of 2 bit RDQI.
         ;-------------------------------------------------------------------

         dum = ISHFT(TEMPORARY(dum),-2)

         ;-------------------------------------------------------------------
         ; Make mask of large fill numbers and dropouts for use later.
         ;-------------------------------------------------------------------

         border_mask = (dum GE 16377)

         ;-------------------------------------------------------------------
         ; Convert DN to radiance. Down-convert Scale_factor and up-convert
         ; dum to FLOAT.
         ;-------------------------------------------------------------------

         dum = TEMPORARY(dum) * FLOAT(Scale_factor)

         ;-------------------------------------------------------------------
         ; Convert radiance to BRF.
         ;-------------------------------------------------------------------

         sizes = SIZE(dum)
         temp_xs = sizes[1]
         temp_ys = sizes[2]
         dum = TEMPORARY(dum) * TEMPORARY(CONGRID(REFORM( $
                            BrfConversionFactors[*,*,iblk]),temp_xs,temp_ys))

         ;-------------------------------------------------------------------
         ; Eliminate large fill numbers and dropouts by masking to -1. If any
         ; half-block has all zeros, then replace them all with -1.
         ;-------------------------------------------------------------------

         dum = TEMPORARY(dum) * (~border_mask)
         border_mask = 0

         ndxs = WHERE(dum[*,0:temp_ys/2-1] NE 0.0, numndxs)
         IF (numndxs EQ 0) THEN dum[*,0:temp_ys/2-1] = -1.0
         ndxs = WHERE(dum[*,temp_ys/2:temp_ys-1] NE 0.0, numndxs)
         IF (numndxs EQ 0) THEN dum[*,temp_ys/2:temp_ys-1] = -1.0
         ndxs = 0

         ;-------------------------------------------------------------------
         ; Pad the block as needed.
         ; The number of times array concatenation is invoked = 
         ; (#block * #block - 1) * #band * #camera (1728 for 7 blocks).
         ;-------------------------------------------------------------------

         numpad = (*blk_pix_offset)[iblk,1] / pad_size
         numrange = range / pad_size

         IF(iblk EQ 0) THEN BEGIN
            FOR j = 0, numpad-1 DO BEGIN
               dum = [dummy,dum]
           ENDFOR
           FOR j = numpad, numrange DO BEGIN
              dum = [dum,dummy]
           ENDFOR
           image = dum
        ENDIF ELSE BEGIN
           FOR j = 0, numpad-1 DO BEGIN
              dum = [dummy,dum]
           ENDFOR
           FOR j = numpad, numrange DO BEGIN
              dum = [dum,dummy]
           ENDFOR
           image = [[image],[dum]]
         ENDELSE

         dum = 0

       ENDFOR  ;  end block loop

       !VAR.RawImages[iband,icam] = PTR_NEW(image)

       image = 0
       dummy = 0
       datablock = 0

    ENDFOR  ;  End band loop

    HDF_SD_END, sd_id

  ENDFOR  ;  End camera loop

  ;--------------------------------------------------------------------------
  ; Store whether this is a global mode (2048 pix-wide in all red bands and
  ; An bands) or local mode orbit (2048 pix-wide in all bands).
  ;--------------------------------------------------------------------------

  !VAR.GlobalLocal = 1
  IF (num_hires_channel EQ 36) THEN !VAR.GlobalLocal = 2

  WIDGET_CONTROL,fetching_tlb,/DESTROY

  ;--------------------------------------------------------------------------
  ; Set some coordinate structure variables.
  ;--------------------------------------------------------------------------

  sizes = SIZE(*!VAR.RawImages[0,4])
  total_xs = sizes[1]
  total_ys = sizes[2]

  CoordStruct.(0).ULCwndwX = 0
  CoordStruct.(0).ULCwndwY = total_ys
  CoordStruct.(0).LRCwndwX = total_xs
  CoordStruct.(0).LRCwndwY = 0
  IF NumCam EQ 18 THEN BEGIN
    CoordStruct.(1).ULCwndwX = 0
    CoordStruct.(1).ULCwndwY = total_ys
    CoordStruct.(1).LRCwndwX = total_xs
    CoordStruct.(1).LRCwndwY = 0
  ENDIF

  ;--------------------------------------------------------------------------
  ; Save the size of a single Nadir block.
  ;--------------------------------------------------------------------------

  CoordStruct.(0).DimCross = nadir_xs
  CoordStruct.(0).DimAlong = nadir_ys
  CoordStruct.(1).DimCross = nadir_xs
  CoordStruct.(1).DimAlong = nadir_ys

  ;--------------------------------------------------------------------------
  ; Retrieve and store the block center time for each block.
  ;--------------------------------------------------------------------------

   fileid = HDF_OPEN(CamFiles[4], /READ)
   vd_ref = HDF_VD_FIND(fileid, 'PerBlockMetadataTime')
   vdata  = HDF_VD_ATTACH(fileid, vd_ref)
   nrec   = HDF_VD_READ(vdata, BlockCntrTime, FIELDS='BlockCenterTime')
   HDF_VD_DETACH, vdata
   HDF_CLOSE, fileid

   BlockCntrTime = STRING(BlockCntrTime)

  ;--------------------------------------------------------------------------
  ; Free some memory.
  ;--------------------------------------------------------------------------

  BrfConversionFactors = 0

  Retval = 0

END ; GetMISRdata

;****************************************************************************
FUNCTION RebinExpand, InArray, XsizeOut, YsizeOut, ExpType
;****************************************************************************
; Expand a 2D array by an integer factor using either nearest neighbor
; sampling or bilinear interpolation. IDL's REBIN procedure produces slightly
; wrong results when interpolating due to assuming array values are pixel
; areas rather than grid nodes. This procedure does it correctly. It's a bit
; slower, though it should be faster than REBIN for large images on machines
; with more than 2 cores. The /CUBIC option is much slower.
;----------------------------------------------------------------------------

size_in = SIZE(InArray)
scale_fctr_x = (size_in[1] - 1.0) / (XsizeOut - 1.0)
scale_fctr_y = (size_in[2] - 1.0) / (YsizeOut - 1.0)

IF (ExpType EQ !KON.Misc.INTERP_TYPE_SAMP) THEN $
   RETURN, REBIN(InArray, XsizeOut, YsizeOut, /SAMPLE)

IF (ExpType EQ !KON.Misc.INTERP_TYPE_BILIN) THEN $
   RETURN, INTERPOLATE(InArray, FINDGEN(XsizeOut) * scale_fctr_x, $
                       FINDGEN(YsizeOut) * scale_fctr_y, /GRID)
                                
IF (ExpType EQ !KON.Misc.INTERP_TYPE_CUBIC) THEN $
   RETURN, INTERPOLATE(InArray, FINDGEN(XsizeOut) * scale_fctr_x, $
                       FINDGEN(YsizeOut) * scale_fctr_y, /CUBIC, /GRID)

END ; RebinExpand

;****************************************************************************
FUNCTION GetRawImage, x1, x2, y1, y2, band, cam, out_res, INTERP_TYPE
;****************************************************************************
; Get all or a portion of the radiance (BRF) data for a particular camera and
; band. This function is necessary, because the data grids are at different
; resolutions for different channels. If the value of x1 is -1, then the
; entire array is returned. Parameter "bilinear_interp" is used only to count
; the number of passed parameters. If an 8th parameter is passed, then any
; increase in resolution is performed by bilinear interpolation rather than
; by duplicating the existing values.
; NOTE - this doesn't reduce the resolution of hi-res channels.
;----------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;----------------------------------------------------------------------------
; Get a pointer to the correct channel data and determine the resolution.
;----------------------------------------------------------------------------

IF (CoordStruct.(0).num_band EQ 1) THEN band = 0

pRawData = !VAR.RawImages[band,cam]

sizes = SIZE(*pRawData)

hires = sizes[1] GE !KON.Instr.HI_RES_PIX_CROSS ? 1 : 0

IF (x1 EQ -1) THEN BEGIN
   x1 = 0
   y1 = 0
   IF (hires OR (out_res EQ !KON.Instr.LO_RES_PIX_SIZE)) THEN BEGIN
      x2 = sizes[1] - 1
      y2 = sizes[2] - 1
   ENDIF ELSE BEGIN
      x2 = sizes[1] * 4 - 1
      y2 = sizes[2] * 4 - 1
   ENDELSE

ENDIF ELSE BEGIN
   IF (hires AND (out_res EQ !KON.Instr.LO_RES_PIX_SIZE)) THEN BEGIN
      x1 /= 4
      x2 /= 4
      y1 /= 4
      y2 /= 4
   ENDIF
   IF (x1 LT 0) THEN x1 = 0
   IF (x2 LT 0) THEN x2 = 0
   IF (y1 LT 0) THEN y1 = 0
   IF (y2 LT 0) THEN y2 = 0
   IF (hires OR (out_res EQ !KON.Instr.LO_RES_PIX_SIZE)) THEN BEGIN
      IF (x1 GE sizes[1]) THEN x1 = sizes[1] - 1.
      IF (x2 GE sizes[1]) THEN x2 = sizes[1] - 1.
      IF (y1 GE sizes[2]) THEN y1 = sizes[2] - 1.
      IF (y2 GE sizes[2]) THEN y2 = sizes[2] - 1.
   ENDIF ELSE BEGIN
      IF (x1 GE (sizes[1] * 4)) THEN x1 = sizes[1] * 4 - 1.
      IF (x2 GE (sizes[1] * 4)) THEN x2 = sizes[1] * 4 - 1.
      IF (y1 GE (sizes[2] * 4)) THEN y1 = sizes[2] * 4 - 1.
      IF (y2 GE (sizes[2] * 4)) THEN y2 = sizes[2] * 4 - 1.
   ENDELSE
ENDELSE

;----------------------------------------------------------------------------
; Branch depending on whether the channel data are at high or low resolution.
;----------------------------------------------------------------------------

IF (hires OR (out_res EQ !KON.Instr.LO_RES_PIX_SIZE)) THEN BEGIN
   RETURN, (*pRawData)[x1:x2, y1:y2]

ENDIF ELSE BEGIN
   xsize = x2 - x1 + 1      ; number of small pixels in x direction
   ysize = y2 - y1 + 1      ; number of small pixels in y direction

   xx1 = FIX(x1 / 4)        ; first large pixel in x direction
   xx2 = FIX(x2 / 4)        ; last large pixel in x direction
   yy1 = FIX(y1 / 4)        ; first large pixel in y direction
   yy2 = FIX(y2 / 4)        ; last large pixel in y direction

   xsize2 = xx2 - xx1 + 1   ; number of large pixels in x direction
   ysize2 = yy2 - yy1 + 1   ; number of large pixels in y direction

   temp_raw = RebinExpand((*pRawData)[xx1:xx2, yy1:yy2], xsize2*4, ysize2*4, $
                          INTERP_TYPE)
   xbeg = x1 MOD 4
   ybeg = y1 MOD 4

   RETURN, temp_raw[xbeg:xsize+xbeg-1, ybeg:ysize+ybeg-1]
ENDELSE

END  ;  GetRawImage

;****************************************************************************
FUNCTION GetRawImageAllAt1100, x1, x2, y1, y2, band, cam
;****************************************************************************
; Get all or a portion of the radiance (BRF) data for a particular camera AND
; BAND at 1100 meters resolution! If the value of x1 is -1, then the entire
; array is returned.
;----------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;----------------------------------------------------------------------------
; Compute the output 1100 m pixel ranges.
;----------------------------------------------------------------------------

x1_out = x1 / 4
x2_out = x2 / 4
y1_out = y1 / 4
y2_out = y2 / 4

;----------------------------------------------------------------------------
; Get a pointer to the correct channel data and determine the resolution.
;----------------------------------------------------------------------------

pRawData = !VAR.RawImages[band,cam]

sizes = SIZE(*pRawData)

hires = sizes[1] GE !KON.Instr.HI_RES_PIX_CROSS ? 1 : 0

;----------------------------------------------------------------------------
; Make sure we don't exceed window bounds.
;----------------------------------------------------------------------------

IF (x1 EQ -1) THEN BEGIN
   x1 = 0
   y1 = 0
   x2 = sizes[1] - 1
   y2 = sizes[2] - 1
ENDIF ELSE BEGIN
   IF (x1 LT 0) THEN x1_out = 0
   IF (x2 LT 0) THEN x2_out = 0
   IF (y1 LT 0) THEN y1_out = 0
   IF (y2 LT 0) THEN y2_out = 0
   IF (hires) THEN BEGIN
      IF (x1 GE sizes[1]) THEN x1 = sizes[1] - 1
      IF (x2 GE sizes[1]) THEN x2 = sizes[1] - 1
      IF (y1 GE sizes[2]) THEN y1 = sizes[2] - 1
      IF (y2 GE sizes[2]) THEN y2 = sizes[2] - 1
   ENDIF ELSE BEGIN
      IF (x1 GE sizes[1] * 4) THEN x1 = sizes[1] - 1
      IF (x2 GE sizes[1] * 4) THEN x2 = sizes[1] - 1
      IF (y1 GE sizes[2] * 4) THEN y1 = sizes[2] - 1
      IF (y2 GE sizes[2] * 4) THEN y2 = sizes[2] - 1
   ENDELSE
ENDELSE

;----------------------------------------------------------------------------
; Return the low-res data or rebin high-res data to 1100 m and return arrays.
;----------------------------------------------------------------------------

IF (~hires) THEN BEGIN

   RETURN, (*pRawData)[x1_out:x2_out, y1_out:y2_out]

ENDIF ELSE BEGIN

   xsize = x2_out - x1_out + 1
   ysize = y2_out - y1_out + 1

   new_ary = (*pRawData)[x1:x2, y1:y2]

   ;-------------------------------------------------------------------------
   ; Rebinning turns any new bin into NAN if any value in the old bins
   ; comprising it are NAN, so work around this.
   ;-------------------------------------------------------------------------

   ndxbad = WHERE(new_ary LE 0.0, numbad)

   IF (numbad EQ 0) THEN BEGIN
      new_ary = REBIN(TEMPORARY(new_ary), xsize, ysize)
   ENDIF ELSE BEGIN
      new_ary[ndxbad] = !VALUES.F_NAN
      new_ary = REFORM(TEMPORARY(new_ary), 4, xsize, 4, ysize, /OVERWRITE) 
      sumFinite = TOTAL(TOTAL(new_ary, 3, /NaN), 1, /NaN)
      nFinite = TOTAL(TOTAL(Finite(new_ary), 3, /NaN), 1, /NaN)
      ndxs = WHERE(nFinite LE 0, numndxs)
      IF (numndxs GT 0) THEN nFinite[ndxs] = 1.0
      ndxs = 0
      new_ary = sumFinite / nFinite
      sumFinite = 0
      nFinite = 0
   ENDELSE

   ndxbad = 0
   
   RETURN, new_ary

ENDELSE

END  ;  GetRawImageAllAt1100

;****************************************************************************
PRO EventHandler_wVeryTop, Event
;****************************************************************************
; Event handler for wVeryTopBase. Handles resizing and destroy events.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE, HIDDEN

IF (WIDGET_INFO(Event.TOP, /UNAME) NE 'VERY_TOP_BASE') THEN RETURN

;----------------------------------------------------------------------------
; Handle resize events.
;----------------------------------------------------------------------------

IF (TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_BASE') THEN BEGIN
   wAnimateBase = WIDGET_INFO(Event.TOP, /CHILD)
   wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
   
   WIDGET_CONTROL, wTopWorkBase, GET_UVALUE=state
   
   CntlGeom1 = WIDGET_INFO(state.wTopWorkBase, /GEOMETRY)
   window_margins = 2 * CntlGeom1.MARGIN   
   
   CntlGeom1 = WIDGET_INFO(state.wAnimateBase, /GEOMETRY)
   CntlGeom2 = WIDGET_INFO(state.wDrawWindow,  /GEOMETRY)
   delx = CntlGeom1.SCR_XSIZE - CntlGeom2.SCR_XSIZE
   dely = CntlGeom1.SCR_YSIZE - CntlGeom2.SCR_YSIZE
   scroll_width = CntlGeom1.XSIZE - CntlGeom2.SCR_XSIZE

   CntlGeom1 = WIDGET_INFO(state.wControlBaseTop, /GEOMETRY)
   CntlGeom2 = WIDGET_INFO(state.wControlBaseBot, /GEOMETRY)
   minx = (CntlGeom1.XSIZE > CntlGeom2.XSIZE) - scroll_width
   miny = 256
   CntlGeom1 = 0
   CntlGeom2 = 0
   
   draw_xsize = ((Event.X - delx) < (state.sizex + window_margins)) > minx
   draw_ysize = ((Event.Y - dely) < (state.sizey + window_margins)) > miny
   
   wDrawWindow = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='DRAW_WINDOW')
   
   IF (!KON.Misc.MINX_PLATFORM EQ 1) THEN BEGIN
      IF (draw_xsize EQ Event.X - delx AND draw_ysize NE Event.Y - dely) THEN $
         WIDGET_CONTROL, state.wDrawWindow, SCR_XSIZE=draw_xsize, YSIZE=draw_ysize
      IF (draw_xsize NE Event.X - delx AND draw_ysize EQ Event.Y - dely) THEN $
         WIDGET_CONTROL, state.wDrawWindow, XSIZE=draw_xsize, SCR_YSIZE=draw_ysize
      IF (draw_xsize EQ Event.X - delx AND draw_ysize EQ Event.Y - dely) THEN $
         WIDGET_CONTROL, state.wDrawWindow, SCR_XSIZE=draw_xsize, SCR_YSIZE=draw_ysize
      IF (draw_xsize NE Event.X - delx AND draw_ysize NE Event.Y - dely) THEN $
         WIDGET_CONTROL, state.wDrawWindow, XSIZE=draw_xsize, YSIZE=draw_ysize
   ENDIF ELSE BEGIN
      WIDGET_CONTROL, state.wDrawWindow, XSIZE=draw_xsize, YSIZE=draw_ysize
   ENDELSE

   state = 0
ENDIF
   
;----------------------------------------------------------------------------
; Handle destroy MINX events.
;----------------------------------------------------------------------------

IF (TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') THEN $
   WIDGET_CONTROL, /DESTROY, Event.TOP
   
END  ;  EventHandler_wVeryTop

;****************************************************************************
PRO FullAnI, NumCam, CamFiles, CoordStruct, MISR_or_AirMISR, wVeryTopBase, $
             Retval
;****************************************************************************
; This program displays a full resolution, multi-block L1B2 animation of all
; available MISR cameras using an animation widget.  The animation widget
; comes with various animation controls and a color table changer.
; Creation Date: 11/25/03 by M Garay.
; Rewritten: 2/12/04 - 9/12 by DNelson.
;----------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON restore_params, StateRestore

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = 0
!VAR.WORK_WNDW = 0
IF (SIZE(ZOOM_WNDW,/TYPE) EQ 0) THEN ZOOM_WNDW = -1

;--------------------------------------------------------------------------
; When entering FullAnI, the MISR_or_AirMISR flag identifies MISR, AirMISR
; and Restore data types. Restore can be either MISR or AirMISR which will
; be resolved by checking CoordStruct.(whichorbit).OrbitNum.
; Load the MISR or AirMISR data.
;--------------------------------------------------------------------------

restoring_session = (MISR_or_AirMISR EQ -1) ? 1 : 0

IF (MISR_or_AirMISR EQ 0) THEN $
   GetMISRdata, NumCam, CamFiles, CoordStruct, Retval

IF (Retval LT 0) THEN RETURN

IF (MISR_or_AirMISR EQ 1) THEN $
   get_AirMISR_data, CamFiles, CoordStruct, Retval

IF (Retval LT 0) THEN RETURN
   
;--------------------------------------------------------------------------
; If restoring session then MISR_or_AirMISR must be changed from -1 to its
; correct value as determined by CoordStruct.(whichorbit).OrbitNum
; (MISR >= 0, AirMISR = -1).
; Take the orbit number from the first orbit since the orbit value of both
; orbits will have the same meaning with regard to functioning as a flag.
;--------------------------------------------------------------------------

IF restoring_session AND $
   (CoordStruct.(0).OrbitNum EQ -1L) THEN BEGIN
  MISR_or_AirMISR = 1		; AirMISR
ENDIF ELSE BEGIN
  MISR_or_AirMISR = 0		; MISR
ENDELSE

;--------------------------------------------------------------------------
; Title of group of widgets. Take the band index from the first orbit since
; both orbits should have the same band, or band combination.
;--------------------------------------------------------------------------

whichorbit = 0
whichorbit1 = 1

IF (CoordStruct.(whichorbit).num_band EQ 4) THEN BEGIN
   bandname = 'RGB '
ENDIF ELSE BEGIN
   CASE 1 OF
     CoordStruct.(whichorbit).band_ndx[0] EQ 0 : bandname = 'Blue'
     CoordStruct.(whichorbit).band_ndx[0] EQ 1 : bandname = 'Green'
     CoordStruct.(whichorbit).band_ndx[0] EQ 2 : bandname = 'Red'
     CoordStruct.(whichorbit).band_ndx[0] EQ 3 : bandname = 'Nir'
   ENDCASE
ENDELSE
   
CASE 1 OF

  MISR_or_AirMISR EQ 0 : BEGIN

    ; Construct the orbit number component of the window title
    ; depending on whether there are 1 or 2 orbits.

    orbit_text = 'Orbit ' + $
          STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)
    IF (CoordStruct.(whichorbit1).OrbitNum NE 0L) THEN $
       orbit_text += ' and ' + $
             STRTRIM(STRING(CoordStruct.(whichorbit1).OrbitNum),2)

    ; Construct the ellipsoid or terrain component of the title.

    IF (STRMATCH(CamFiles[0], '*ELLIPSOID*')) THEN BEGIN
       type_name = 'Ellipsoid Data - '
    ENDIF ELSE BEGIN
       type_name = 'Terrain Data - '
    ENDELSE

    ; Construct the band component of the title.

    IF (CoordStruct.(whichorbit).num_band EQ 4) THEN BEGIN
       bandname = 'RGB '
    ENDIF ELSE BEGIN
       CASE 1 OF
          CoordStruct.(whichorbit).band_ndx[0] EQ  0 : bandname = 'Blue band '
          CoordStruct.(whichorbit).band_ndx[0] EQ  1 : bandname = 'Green band '
          CoordStruct.(whichorbit).band_ndx[0] EQ  2 : bandname = 'Red band '
          CoordStruct.(whichorbit).band_ndx[0] EQ  3 : bandname = 'Nir band '
       ENDCASE
    ENDELSE
   
    ; Construct the date component of the title.

    orbitdate = ''
    toks = STRSPLIT(CoordStruct.(whichorbit).OrbitDate, '-', $
                    /EXTRACT, COUNT=ntok)
    IF (ntok EQ 3) THEN BEGIN
       months = ['Jan','Feb','Mar','Apr','May','Jun', $
                 'Jul','Aug','Sep','Oct','Nov','Dec']
       month = months[toks[1]-1]
       orbitdate = month + ' ' + $
            STRTRIM(STRING(FIX(toks[2])),2) + ', ' + toks[0]
    ENDIF
    IF (CoordStruct.(whichorbit1).OrbitNum NE 0L) THEN BEGIN
       toks = STRSPLIT(CoordStruct.(whichorbit1).OrbitDate, '-', $
                       /EXTRACT, COUNT=ntok)
       IF (ntok EQ 3) THEN BEGIN
          months = ['Jan','Feb','Mar','Apr','May','Jun', $
                    'Jul','Aug','Sep','Oct','Nov','Dec']
          month = months[toks[1]-1]
          orbitdate += ' and ' + month + ' ' + $
               STRTRIM(STRING(FIX(toks[2])),2) + ', ' + toks[0]
       ENDIF
    ENDIF

    minx_vers = 'MINX Version ' + !KON.Misc.MINX_VERSION_NUM + ' : '
    IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_DEV_VER) THEN $
       minx_vers = 'MINX (development) Version ' + $
          !KON.Misc.MINX_VERSION_NUM + ' : '

    ; Put everything together.

    title = minx_vers + 'MISR ' + bandname + type_name + 'Path ' + $
            STRTRIM(STRING(CoordStruct.(whichorbit).PathNum),2) + ' - ' + $
            orbit_text + ' - ' + 'Blocks ' + $
            STRTRIM(STRING(CoordStruct.(whichorbit).BlkBeg),2) + ' to ' + $
            STRTRIM(STRING(CoordStruct.(whichorbit).BlkEnd),2) + ' - ' + $
            orbitdate
  END

  MISR_or_AirMISR EQ 1 : title = 'AirMISR ' + bandname + 'Band'

ENDCASE
   
;--------------------------------------------------------------------------
; If restoring session then check if an existing animation widget exists
; and if so then destroy the widget tree prior to creating a new animation
; widget with the restored data. There will not be a widget tree to kill if
; a .sav file type is selected on startup.
;--------------------------------------------------------------------------
 
IF (restoring_session) THEN BEGIN
  IF (SIZE(TLB, /TYPE) EQ 3) THEN BEGIN
    IF (WIDGET_INFO(TLB, /VALID_ID)) THEN BEGIN

      ;--------------------------------------------------------------------
      ; Kill off the BRF plot window, if it exists.
      ;--------------------------------------------------------------------

      SafeWDELETE,ZOOM_WNDW, didit
      ZOOM_WNDW = -1

      ;--------------------------------------------------------------------
      ; Kill off the animation widget, if it exists.
      ;--------------------------------------------------------------------

      WIDGET_CONTROL, TLB, /DESTROY

    ENDIF
  ENDIF
ENDIF

;--------------------------------------------------------------------------
; Create a base widget for the very top level window.
;--------------------------------------------------------------------------

wVeryTopBase = WIDGET_BASE(TITLE=title, UNAME='VERY_TOP_BASE', $
                           /TLB_SIZE_EVENTS, /KBRD_FOCUS_EVENTS, $
                           TLB_FRAME_ATTR=0) 
TLB = wVeryTopBase
   
;--------------------------------------------------------------------------
; Take the information from the first orbit since both orbits should have
; identical information in this case. Create the children. wAnimateBase
;--------------------------------------------------------------------------

ncam_plus = NumCam + 1    ; add a fake camera in position 0 for a work area

wAnimateBase = CW_ANIMATE(wVeryTopBase, CoordStruct.(whichorbit).LRCwndwX, $
                          CoordStruct.(whichorbit).ULCwndwY, /CYCLE, $
                          ncam_plus, CoordStruct.(whichorbit).add_edge)

WIDGET_CONTROL, /REALIZE, wVeryTopBase

;--------------------------------------------------------------------------
; If this is a restore session, put the state structure back in the
; animation window and set buttons appropriately.
;--------------------------------------------------------------------------

IF (restoring_session) THEN BEGIN
   wTopWorkBase = WIDGET_INFO(wAnimateBase, FIND_BY_UNAME='TOP_WORK_BASE')
   WIDGET_CONTROL, wTopWorkBase, GET_UVALUE = state, /NO_COPY

   state.nframes     = StateRestore.nframes
   state.curframe    = StateRestore.curframe
   state.begframe    = StateRestore.begframe
   state.endframe    = StateRestore.endframe

   state.showBRFwndw = StateRestore.showBRFwndw
   state.showdots    = StateRestore.showdots
   state.showcountry = StateRestore.showcountry
   state.showLLgrid  = StateRestore.showLLgrid
   state.showobjects = StateRestore.showobjects
   state.showmapclr  = StateRestore.showmapclr
   state.showfire    = StateRestore.showfire
   state.showmarker  = StateRestore.showmarker

   IF (state.showBRFwndw) THEN BEGIN
      WIDGET_CONTROL, state.wFramesBrfButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesBrfButton, SET_BUTTON = 1
      WIDGET_CONTROL, state.wBrfParamsButton, SENSITIVE = 1
   ENDIF
   ZOOM_WNDW = -1
   
   IF (state.showdots) THEN BEGIN
      WIDGET_CONTROL, state.wFramesDotsButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesDotsButton, SET_BUTTON = 1
   ENDIF
   
   IF (state.showcountry) THEN BEGIN
      WIDGET_CONTROL, state.wFramesCountryBtn, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesCountryBtn, SET_BUTTON = 1
   ENDIF
   
   IF (state.showLLgrid) THEN BEGIN
      WIDGET_CONTROL, state.wFramesLLgridBtn, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesLLgridBtn, SET_BUTTON = 1
   ENDIF
   
   IF (!VAR.CurrFiles.TerrHoles_Loaded) THEN BEGIN
      WIDGET_CONTROL, state.wFramesHolesButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesHolesButton, SET_BUTTON = 1
   ENDIF
   
   IF (state.showobjects) THEN BEGIN
      WIDGET_CONTROL, state.wFramesLinesButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesLinesButton, SET_BUTTON = 1
   ENDIF
 
   IF (state.showmapclr) THEN BEGIN
      WIDGET_CONTROL, state.wFramesPixColorBtn, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesPixColorBtn, SET_BUTTON = 1
   ENDIF

   IF (!VAR.CurrFiles.Fire_Loaded) THEN BEGIN
      WIDGET_CONTROL, state.wFramesFireButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesFireButton, SET_BUTTON = 1
   ENDIF
   
   IF (!VAR.CurrFiles.Marker_Loaded) THEN BEGIN
      WIDGET_CONTROL, state.wFramesMarkerButton, SENSITIVE = 1
      WIDGET_CONTROL, state.wFramesMarkerButton, SET_BUTTON = 1
   ENDIF

   WIDGET_CONTROL, state.wFramesProductMenu, SENSITIVE = 1
   WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE = $
          (!VAR.DataSwath.DATA_TYPE NE !KON.DataProd.TYPE_NONE) ? 1 : 0

   WIDGET_CONTROL, state.wScalingMethodBtn, SET_BUTTON = 1
    
   WIDGET_CONTROL, state.wContrastCntlSlider, SENSITIVE = 1
   WIDGET_CONTROL, state.wBrightCntlSlider, SENSITIVE = 1

   WIDGET_CONTROL, wTopWorkBase, SET_UVALUE = state, /NO_COPY
      
ENDIF ELSE BEGIN

   ;-----------------------------------------------------------------------
   ; Create the work window arrays.
   ;-----------------------------------------------------------------------

   sizes = SIZE(*!VAR.RawImages[0,4])
   total_xs = sizes[1]
   total_ys = sizes[2]

   WorkImage = FLTARR(total_xs, total_ys, 3)
   WORK_MinMax = FLTARR(3,2)	                ; (rgb,minmax)

   ;-----------------------------------------------------------------------
   ; Set the default color scaling if not a restore.
   ;-----------------------------------------------------------------------

   !VAR.ClrScaleVals.BandDisplayOpt = (CoordStruct.(0).num_band EQ 1) ? 4 : 1
ENDELSE
   
;--------------------------------------------------------------------------
; Create an image array and load the animation. First get the saved slider
; values for brightness and contrast if the session is being restored from
; a .sav file. Otherwise use default values.
;--------------------------------------------------------------------------

HELP, /STRUCTURE, state, OUTPUT=exists

IF (~STRMATCH(exists[0], '*UNDEFINED = <Undefined>')) THEN BEGIN
   slider_cntl1 = FIX(!VAR.ClrScaleVals.Contrast   * 100)
   slider_cntl2 = FIX(!VAR.ClrScaleVals.Brightness * 100)
   slider_val1  = STRING(!VAR.ClrScaleVals.Contrast, FORMAT='(F4.2)')
   slider_val2  = STRING(!VAR.ClrScaleVals.Brightness, FORMAT='(F4.2)')

   WIDGET_CONTROL, state.wScalingMethodBtn, SET_BUTTON = 1
    
   WIDGET_CONTROL, state.wContrastCntlSlider, SET_VALUE = slider_cntl1
   WIDGET_CONTROL, state.wContrastValue, SET_VALUE = slider_val1

   WIDGET_CONTROL, state.wBrightCntlSlider, SET_VALUE = slider_cntl2
   WIDGET_CONTROL, state.wBrightnessValue, SET_VALUE = slider_val2
ENDIF

exists = 0
numband = CoordStruct.(whichorbit).num_band
   
;--------------------------------------------------------------------------
; Define swath edges by the first and last valid pixels in each row. Use
; this to mask out all values outside the edges to -1. This is because data
; holes are present outside the swath as well as inside. This must be done
; for ellipsoid data also.
;--------------------------------------------------------------------------

FOR icam=0,NumCam-1 DO BEGIN
   FOR iband=0,numband-1 DO BEGIN

      pImage = !VAR.RawImages[iband,icam]
      sizes = SIZE(*pImage)

      FOR ilin=0,sizes[2]-1 DO BEGIN

         ndxs = WHERE((*pImage)[*,ilin] GT 0.0, numndxs)

         IF (numndxs GT 0) THEN BEGIN
            minndx = MIN(ndxs, MAX=maxndx)
            IF (minndx GT 0) THEN (*pImage)[0:minndx-1, ilin] = -1.0
            IF (maxndx LT sizes[1]-1) THEN $
               (*pImage)[maxndx+1:sizes[1]-1, ilin] = -1.0
         ENDIF
      ENDFOR

   ENDFOR
ENDFOR

ndxs = 0

;--------------------------------------------------------------------------
; Estimate BRF values to use in filling missing pixels inside the swath.
;--------------------------------------------------------------------------

IF (STRMATCH(CamFiles[0], '*TERRAIN*')) THEN BEGIN
   SaveMissingPixelsInImage, NumCam, numband, Retval
   !VAR.L1B2_TYPE = 'TERRAIN'
ENDIF
   
;--------------------------------------------------------------------------
; Scale the colors.
;--------------------------------------------------------------------------

;PROFILER, 'HISTOGRAM'
;PROFILER, /SYSTEM

FOR icam = 1, ncam_plus-1 DO BEGIN
   WIDGET_CONTROL, /HOURGLASS
   use_bands = (numband EQ 1) ? [0] : [0,1,2,3]
   PrepImageArray, 1, use_bands, NumCam, icam-1, image, Retval
   IF (Retval LT 0) THEN RETURN
   max_band = (numband EQ 1) ? 0 : 2
   CW_ANIMATE_LOAD, wAnimateBase, FRAME=icam, IMAGE=image[*,*,0:max_band], /ORDER
   image = 0
ENDFOR

;PROFILER, /REPORT

;--------------------------------------------------------------------------
; Prep the channel 0 work array.
;--------------------------------------------------------------------------

image_size = SIZE(WorkImage)
image = BYTARR(image_size[1],image_size[2],image_size[3])
image[*,*,0] = BYTSCL(WorkImage[*,*,0],MIN=WORK_MinMax[0,0], $
                      Max=WORK_MinMax[0,1],/NAN)
image[*,*,1] = BYTSCL(WorkImage[*,*,1],MIN=WORK_MinMax[1,0], $
                      Max=WORK_MinMax[1,1],/NAN)
image[*,*,2] = BYTSCL(WorkImage[*,*,2],MIN=WORK_MinMax[2,0], $
                      Max=WORK_MinMax[2,1],/NAN)

;--------------------------------------------------------------------------
; Load the work image. CW_ANIMATE_RUN sets the default frame rate.
;--------------------------------------------------------------------------

CW_ANIMATE_LOAD, wAnimateBase, FRAME=0, IMAGE=image[*,*,0:max_band], /ORDER
CW_ANIMATE_RUN, wAnimateBase, 20

image = 0
   
;--------------------------------------------------------------------------
; Done waiting. If you wish to display other interactive apps for user to
; manipulate, e.g. the height/wind data cube or an iTool, use /NO_BLOCK on
; main window. This may fail anyway.
; XMANAGER options follow.
;--------------------------------------------------------------------------

; Use this for production.
XMANAGER, 'CW_ANIMATE', wVeryTopBase, EVENT_HANDLER = 'EventHandler_wVeryTop'

; Use this for some debugging.
;  XMANAGER, EVENT_HANDLER = 'EventHandler_wVeryTop', CATCH=0

; Use this for testing display of 3D tool.
;  XMANAGER, 'CW_ANIMATE', wVeryTopBase, EVENT_HANDLER = 'EventHandler_wVeryTop', $
;            /NO_BLOCK

Retval = 1
   
END ; FullAnI

;****************************************************************************
PRO CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Retval
;****************************************************************************
; If the full product isn't available, look for subsetted files. Let user
; retry if the selected file isn't found or if the block range of the found
; file doesn't include the requested range.
;----------------------------------------------------------------------------

Retval = -1

AnFileSave = AnFile

ipos = STRPOS(AnFileSave, '.hdf')
IF (ipos LT 0) THEN RETURN
AnFileSave = STRMID(AnFileSave, 0, ipos) + '.b*-*.hdf'

filnams = FILE_SEARCH(BasePath + AnFileSave, COUNT=numfil)

IF (numfil EQ 1) THEN AnFileSave = filnams[0]
IF (numfil EQ 0) THEN BEGIN
   Retval = -2
   RETURN
ENDIF

IF (numfil GE 1) THEN BEGIN
   FOR ifil=0,numfil-1 DO BEGIN
      toks = STRSPLIT(filnams[ifil], '.', /EXTRACT, COUNT=numpart)
      IF (numpart EQ 2) THEN BEGIN
         AnFileSave = filnams[ifil]
         BREAK
      ENDIF ELSE BEGIN
         IF (numpart EQ 3) THEN BEGIN
            blks = STRSPLIT(toks[1], '-', /EXTRACT, COUNT=numblk)
            numchr = STRLEN(blks[0])
            blk1 = FIX(STRMID(blks[0], 1, numchr-1))
            blk2 = FIX(blks[1])
            IF (BlkBeg GE blk1 AND BlkEnd LE blk2) THEN BEGIN
               AnFileSave = filnams[ifil]
               BREAK
            ENDIF ELSE BEGIN
               Retval = -3
               IF (ifil EQ numfil-1) THEN RETURN
               CONTINUE
            ENDELSE
         ENDIF ELSE BEGIN
            mssg = ['If your directory name contains a dot (.)', $
                    'you must remove it before proceeding.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            Retval = -2
            RETURN
         ENDELSE
      ENDELSE
   ENDFOR
ENDIF

ipos = STRPOS(AnFileSave, !KON.Misc.Slash, /REVERSE_SEARCH)
AnFile = STRMID(AnFileSave, ipos+1)

Retval = 0

END  ;  CheckForSubsetting

;****************************************************************************
PRO GetMisrBasePath, RadianceDir, AnFile, PathStr, OrbitStr, BlkBeg, BlkEnd, $
                     BasePath, Retval
;****************************************************************************
; Use the name provided on line 1 of the PlumeProjOrbitList file to find the
; location of level 1 camera files and to construct the name of the base path
; where they are located. This needs to be flexible so the user can enter a
; single directory yet have MINX find camera files in any of several possible
; locations. In the following, <cam> refers to the 9 camera subdirectories
; named [AA,AF,AN,BA,BF,CA,CF,DA,DF]. If, for example, the directory name
; specified on line 1 of PlumeProjOrbitList is /name1/name2/<last_component>/,
; then:
; 1) If last_component does not contain the text GRP_TERRAIN or GRP_ELLIPSOID,
;    then the 9 camera files must be in either:
;  a) ./<last_component>/ or
;  b) ./<last_component>/<cam>/.
; 2) If last_component does contain the text GRP_TERRAIN or GRP_ELLIPSOID,
;    then the 9 camera files must be in one of:
;  a) ./<last_component>/ or
;  b) ./<last_component>/<cam>/ or
;  c) /<last_component>/database/<cam>/ or
;  d) /<last_component>/database/path<xxx>/<cam>/ where <xxx> is the 3-digit
;     path number of the orbit in question.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = 0
FinalStatus = -1

;----------------------------------------------------------------------------
; Test if the name of the directory is empty.
;----------------------------------------------------------------------------

toks = STRSPLIT(RadianceDir, !KON.Misc.Slash, COUNT=numtok, /EXTRACT)

IF (numtok EQ 0) THEN BEGIN
   mssg = 'The directory location where MISR camera files are located ' + $
          'is an empty string.'
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   Retval = -1
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Test if the data are in the directory exactly as passed. If not, check if
; a subsetted version of the file with the correct blocks is.
;----------------------------------------------------------------------------

BasePath = RadianceDir
;IF (FILE_SEARCH(BasePath + AnFile, /TEST_REGULAR, COUNT=numfil)) THEN RETURN
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Status
IF (Status LT FinalStatus) THEN FinalStatus = Status
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

;----------------------------------------------------------------------------
; Otherwise test if the data are in the passed subdirectory with the camera
; subdirectory added.
;----------------------------------------------------------------------------

BasePath = RadianceDir + 'AN' + !KON.Misc.Slash
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Status
IF (Status LT FinalStatus) THEN FinalStatus = Status
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

;----------------------------------------------------------------------------
; Otherwise test if the data are in the passed directory with the path name
; subdirectory added.
;----------------------------------------------------------------------------

BasePath = RadianceDir + 'path' + PathStr + !KON.Misc.Slash
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Status
IF (Status LT FinalStatus) THEN FinalStatus = Status
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

;----------------------------------------------------------------------------
; Otherwise test if the data are in the passed directory with the path name
; subdirectory and camera subdirectory added.
;----------------------------------------------------------------------------

BasePath = RadianceDir + 'path' + PathStr + !KON.Misc.Slash + 'AN' + !KON.Misc.Slash
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Status
IF (Status LT FinalStatus) THEN FinalStatus = Status
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

;----------------------------------------------------------------------------
; Otherwise test if the data are in the passed directory with the subdirectory
; name ""database" added and path name subdirectory and camera subdirectory
; added.
;----------------------------------------------------------------------------

BasePath = RadianceDir + 'database' + !KON.Misc.Slash + 'path' + PathStr + $
           !KON.Misc.Slash + 'AN' + !KON.Misc.Slash
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

CheckForSubsetting, BasePath, AnFile, BlkBeg, BlkEnd, Status
IF (Status LT FinalStatus) THEN FinalStatus = Status
IF (FILE_TEST(BasePath + AnFile, /REGULAR)) THEN RETURN

;----------------------------------------------------------------------------
; Show messages here.
;----------------------------------------------------------------------------

Retval = -1

IF (FinalStatus EQ -3) THEN BEGIN
   mssg = ['A MISR L1B2 An camera file for your orbit was found, but it', $
           'is subsetted and does not contain your requested block range.', $
           ' ', 'Try again or exit.']
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
ENDIF

IF (FinalStatus EQ -2) THEN BEGIN
   mssg = ['The MISR L1B2 An camera file you requested is not available.', $
           ' ', 'Try again or exit.']
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
ENDIF

END  ;  GetMisrBasePath

;****************************************************************************
PRO ReadOrbitParams, OrbitNum, BlkBeg, BlkEnd, L1B2AnFile, plume_out_dir, $
                     Retval
;****************************************************************************
; Read the block range and complete terrain or ellipsoid file name from a
; file in the user's home directory. Display the orbit numbers and block
; ranges in a dialog box. The user selects an orbit/block range which is then
; loaded by caller.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

num_entry = 0
rad_dir_ter = ''
rad_dir_ell = ''
rad_ver = ''
rad_terrain_begname = 'MISR_AM1_GRP_TERRAIN_GM_P'
rad_ellipse_begname = 'MISR_AM1_GRP_ELLIPSOID_GM_P'

rad_dirpart = !KON.Misc.Slash + 'database' + !KON.Misc.Slash

;----------------------------------------------------------------------------
; Before start-up, the user's PlumeProject directory must contain a file named
; "PlumeProjOrbitList.txt" with a 3-line header consisting of
; 1) directory for input TERRAIN and/or ELLIPSOID files;
; 2) version string for TERRAIN and ELLIPSOID files;
; 3) directory for writing output data and images.
; Each successive line contains information for one orbit in this order in
; free format: OrbitNum BlkBeg BlkEnd Comments (optional).
;----------------------------------------------------------------------------

proj_file_name = 'PlumeProjOrbitList.txt'
list_file_name = !SAV.PlumeProjDir + proj_file_name
UserError = 0

retry_select:
IF (~ FILE_TEST(list_file_name, /READ, /REGULAR) OR UserError) THEN BEGIN
   msg = $
    ['A ' + proj_file_name + ' file was not found in your Plume Project Directory at ', $
     'this location: ' + list_file_name + ',', $
     'or it has the wrong format.', ' ', $
     'Possible solutions are :', $
     '  1) Create or copy the file manually from another location;', $
     '  2) Move a file created by the "MINX Plume Project Utilities" into your Plume', $
     '     Project Directory;', $
     '  3) Change your Plume Project Directory in the Plume Project Preferences dialog', $
     '     to the location of your file.', ' ', $
     'The Plume Project Preferences dialog will appear after you dismiss this message.', $
     ' ', ' ', $
     
     'The ' + proj_file_name + ' file must contain 3 lines of header followed by a list', $
     'of orbits to choose for processing. Do not create this file with an editor that', $
     'inserts invisible formatting characters.', ' ', $
     'The header must consist of these 3 lines:', $
     '1) One or two directory names where MISR GRP_TERRAIN and GRP_ELLIPSOID files', $
     '   reside. Use two names in the order TERRAIN ELLIPSOID if you need to use both', $
     '   files types AND if they are stored in different locations. Separate the names', $
     '   by at least one space character or tab.', $
     '2) Version string for TERRAIN and/or ELLIPSOID files (e.g. F03_0024).', $
     '3) Directory name where MINX output data and images will be written.', ' ', $
     'Each successive line contains information for one orbit in this order in free', $
     'format with items separated by space characters or tabs:', $
     '   OrbitNumber  BeginBlockNumber  EndBlockNumber  Comments', ' ', $
     'The comments field may contain spaces and is optional.', $
     'You may also include blank lines in the orbit list.']
   ret = DIALOG_MESSAGE(msg, /ERROR, /CENTER)
   PlumeProj_Preferences_gui, Cancel
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Open the file and read through once to count the entries and to get the
; information in header lines including the location of level 1 radiance
; files and the location where output data will be written.
;----------------------------------------------------------------------------

OPENR, unit, list_file_name, /GET_LUN
BegMidEnd = 0
buff = ''

ReadNextAsciiLine, unit, BegMidEnd, buff
vals = STRSPLIT(buff, ' ' + STRING(9b), COUNT=num_ent, /EXTRACT)
IF (num_ent NE 1 AND num_ent NE 2) THEN BEGIN 
   FREE_LUN, unit
   UserError = 1
   GOTO, retry_select
ENDIF

rad_dir_ter = vals[0]
ilen = STRLEN(rad_dir_ter)
IF (STRMID(rad_dir_ter, ilen-1, 1) NE !KON.Misc.Slash) THEN $
   rad_dir_ter += !KON.Misc.Slash

IF (num_ent EQ 1) THEN BEGIN
rad_dir_ell = rad_dir_ter
ENDIF ELSE BEGIN
   rad_dir_ell = vals[1]
   ilen = STRLEN(rad_dir_ell)
   IF (STRMID(rad_dir_ell, ilen-1, 1) NE !KON.Misc.Slash) THEN $
      rad_dir_ell += !KON.Misc.Slash
ENDELSE

ReadNextAsciiLine, unit, BegMidEnd, rad_ver

ReadNextAsciiLine, unit, BegMidEnd, buff
vals = STRSPLIT(buff, ' ' + STRING(9b), COUNT=num_ent, /EXTRACT)
plume_out_dir = buff
ilen = STRLEN(plume_out_dir)
IF (STRMID(plume_out_dir, ilen-1, 1) NE !KON.Misc.Slash) THEN $
   plume_out_dir += !KON.Misc.Slash

WHILE (~ EOF(unit)) DO BEGIN
   ReadNextAsciiLine, unit, BegMidEnd, buff
   num_entry += 1
ENDWHILE

;----------------------------------------------------------------------------
; Allocate arrays for storing the orbit list data.
;----------------------------------------------------------------------------

orbit   = STRARR(num_entry)
blk_beg = STRARR(num_entry)
blk_end = STRARR(num_entry)
commstr = STRARR(num_entry)

;----------------------------------------------------------------------------
; Now rewind and read in all the entries. Test that the orbit number and block
; range parameters are all numeric and in range. Construct a list containing
; only valid orbit numbers and block ranges.
;----------------------------------------------------------------------------

POINT_LUN, unit, 0
BegMidEnd = 0

ReadNextAsciiLine, unit, BegMidEnd, buff
ReadNextAsciiLine, unit, BegMidEnd, buff
ReadNextAsciiLine, unit, BegMidEnd, buff

num_good_entry = 0

FOR ientry=0,num_entry-1 DO BEGIN
   ReadNextAsciiLine, unit, BegMidEnd, buff
   vals = STRSPLIT(buff, ' ' + STRING(9b), COUNT=num_ent, /EXTRACT)

   orbit[num_good_entry]   = ''
   blk_beg[num_good_entry] = ''
   blk_end[num_good_entry] = ''
   commstr[num_good_entry] = ''
   numndxs1 = 0
   numndxs2 = 0
   numndxs3 = 0

   IF (num_ent EQ 0) THEN BEGIN
      IF (num_good_entry NE 0) THEN num_good_entry += 1 
   ENDIF ELSE BEGIN
      IF (num_ent GE 3) THEN BEGIN
         bytes = BYTE(vals[0])
         ndxs = WHERE(bytes LT 48 OR bytes GT 57, numndxs1)
         bytes = BYTE(vals[1])
         ndxs = WHERE(bytes LT 48 OR bytes GT 57, numndxs2)
         bytes = BYTE(vals[2])
         ndxs = WHERE(bytes LT 48 OR bytes GT 57, numndxs3)
         ndxs = 0

         IF (numndxs1 EQ 0 AND numndxs2 EQ 0 AND numndxs3 EQ 0) THEN BEGIN

            IF (LONG(vals[0]) GE 995 AND $
                LONG(vals[0]) LE LONG(!KON.Misc.LARGE_POS_NUM) AND $
                FIX(vals[1]) GE 1 AND FIX(vals[1]) LE !KON.Instr.NUM_BLOCKS AND $
                FIX(vals[2]) GE 1 AND FIX(vals[2]) LE !KON.Instr.NUM_BLOCKS AND $
                FIX(vals[2]) GE FIX(vals[1])) THEN BEGIN
               orbit[num_good_entry] = vals[0]
               IF (STRLEN(orbit[num_good_entry]) LT 5) THEN $
                  orbit[num_good_entry] = ' ' + orbit[num_good_entry]
               blk_beg[num_good_entry] = vals[1]
               IF (STRLEN(blk_beg[num_good_entry]) EQ 1) THEN $
                  blk_beg[num_good_entry] = '  ' + blk_beg[num_good_entry]
               IF (STRLEN(blk_beg[num_good_entry]) EQ 2) THEN $
                  blk_beg[num_good_entry] = ' '  + blk_beg[num_good_entry]
               blk_end[num_good_entry] = vals[2]
               IF (STRLEN(blk_end[num_good_entry]) EQ 1) THEN $
                  blk_end[num_good_entry] = '  ' + blk_end[num_good_entry]
               IF (STRLEN(blk_end[num_good_entry]) EQ 2) THEN $
                  blk_end[num_good_entry] = ' '  + blk_end[num_good_entry]

               IF (num_ent GT 3) THEN BEGIN
                  commpos = STRPOS(buff, vals[3])
                  commstr[num_good_entry] = STRMID(buff, commpos)
               ENDIF

               num_good_entry += 1 

            ENDIF
         ENDIF
      ENDIF
   ENDELSE
ENDFOR

FREE_LUN, unit

num_entry = num_good_entry

;----------------------------------------------------------------------------
; Show a list of orbits in a dialog box and let user select one.
;----------------------------------------------------------------------------

orbit_list = STRARR(num_entry)

FOR ientry=0,num_entry-1 DO BEGIN
   IF (STRLEN(orbit[ientry]) EQ 0) THEN BEGIN
      orbit_list[ientry] = ' '
   ENDIF ELSE BEGIN
      orbit_list[ientry] = ' orbit ' + orbit[ientry] + ' - blocks ' + $
      blk_beg[ientry] + ' to ' + blk_end[ientry] + '  : ' + commstr[ientry] + ' '
   ENDELSE
ENDFOR

reshow_list:
reduce_blks = 0

ProcessPlumeOrbits_gui, orbit_list, iorbit, reduce_blks, terrain_ellipse

IF (iorbit LT 0) THEN RETURN

;----------------------------------------------------------------------------
; Retry if user picked an invalid line.
;----------------------------------------------------------------------------

IF (orbit[iorbit] EQ '' OR blk_beg[iorbit] EQ '' OR $
    blk_end[iorbit] EQ '') THEN BEGIN
   mssg = ['You have selected a separator line', $
           'or a line with invalid data.', ' ', 'Try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   GOTO, reshow_list
ENDIF

;----------------------------------------------------------------------------
; Set either the terrain or the ellipsoid path depending on which button the
; user selected.
;----------------------------------------------------------------------------

IF (terrain_ellipse EQ 0) THEN BEGIN
   rad_dir = rad_dir_ter
   rad_begname = rad_terrain_begname
ENDIF
IF (terrain_ellipse EQ 1) THEN BEGIN
   rad_dir = rad_dir_ell
   rad_begname = rad_ellipse_begname
ENDIF

;----------------------------------------------------------------------------
; Convert first 3 required params to numeric form as well as strings that
; have the correct number of characters.
;----------------------------------------------------------------------------

OrbitNum = LONG(orbit[iorbit])
BlkBeg   = FIX(blk_beg[iorbit])
BlkEnd   = FIX(blk_end[iorbit])

IF (reduce_blks AND BlkEnd GT BlkBeg) THEN BEGIN
   BlkBeg += 1
   BlkEnd -= 1
   IF (BlkEnd LT BlkBeg) THEN BlkEnd = BlkBeg
ENDIF

spath = PathFromOrbit(LONG(OrbitNum))
spath = STRTRIM(STRING(spath),2)
IF (STRLEN(spath) EQ 1) THEN spath = '00' + spath
IF (STRLEN(spath) EQ 2) THEN spath = '0'  + spath

sorbit = STRTRIM(STRING(OrbitNum),2)
IF (STRLEN(sorbit) EQ 3) THEN sorbit = '000' + sorbit
IF (STRLEN(sorbit) EQ 4) THEN sorbit = '00'  + sorbit
IF (STRLEN(sorbit) EQ 5) THEN sorbit = '0'   + sorbit

;----------------------------------------------------------------------------
; Create the An camera file name.
; Remove the 'F' in version name so we can put a wild card there to include
; possible local mode products. 
;----------------------------------------------------------------------------

;rad_len = STRLEN(rad_ver)
;rad_ver = STRMID(rad_ver, 1)
;L1B2AnFile = rad_begname + spath + '_O' + sorbit + '_AN_*' + rad_ver + '.hdf'
L1B2AnFile = rad_begname + spath + '_O' + sorbit + '_AN_' + rad_ver + '.hdf'

;----------------------------------------------------------------------------
; Find the base path to the 9 camera files used as imagery input and create
; the full path name.
;----------------------------------------------------------------------------

GetMisrBasePath, rad_dir, L1B2AnFile, spath, sorbit, BlkBeg, BlkEnd, $
                 base_path, status

IF (status LT 0) THEN GOTO, reshow_list

L1B2AnFile = base_path + L1B2AnFile

;----------------------------------------------------------------------------
; Clean up
;----------------------------------------------------------------------------

orbit = 0
orbit_list = 0
blk_beg = 0
blk_end = 0
commstr = 0
terr_file = 0

Retval = 0

END  ;  ReadOrbitParams

;****************************************************************************
PRO FillCoordStruct, OrbitNum, BlkBeg, BlkEnd, L1B2_Files, NumCam,$
                     MISR_or_AirMISR, plume_out_dir, Retval
;****************************************************************************
; Fill the CoordStruct with values from a special file. Used only for the
; Plume Processing option, so never uses 2 orbits of data.
;----------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = 0

;----------------------------------------------------------------------------
; Read orbit information from the orbit list file.
;----------------------------------------------------------------------------

ReadOrbitParams, OrbitNum, BlkBeg, BlkEnd, L1B2_Files, plume_out_dir, Retval
IF (Retval LT 0) THEN RETURN

;----------------------------------------------------------------------------
; make sure the An camera file exists.
;----------------------------------------------------------------------------

rval = FILE_TEST(L1B2_Files, /NOEXPAND_PATH, /READ, /REGULAR)
IF (rval NE 1) THEN BEGIN
   mssg = ['This file is missing:', L1B2_Files, $
           'Fix the problem and try running again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   Retval = -1
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Use the An camera file to get other needed information.
;----------------------------------------------------------------------------

l1b2_files_temp = STRARR(!KON.Instr.NCAM)
l1b2_files_temp[0] = L1B2_Files

GetCamFiles, 0, l1b2_files_temp, OrbitNum, 0, !KON.Instr.NCAM, 0, $
             [1,1,1,1,1,1,1,1,1], Retval
!VAR.CurrFiles.CamFiles[0:8] = l1b2_files_temp[0:8]

coord_struct = CoordStruct
GetOrbitParams_part1, l1b2_files_temp, coord_struct, 0, BlockULCx, $
                      BlockULCy, BlockLRCx, BlockLRCy, PathNum, Retval
CoordStruct = coord_struct
                      
IF (Retval LT 0) THEN RETURN

L1B2_Files = l1b2_files_temp

CoordStruct.(0).BlkBeg = BlkBeg
CoordStruct.(0).BlkEnd = BlkEnd

GetOrbitParams_part2, CoordStruct, Retval, 0, BlockULCx, BlockULCy, $
                      BlockLRCx, BlockLRCy

NumCam = N_ELEMENTS(l1b2_files_temp)
MISR_or_AirMISR = 0

CoordStruct.(0).OrbitNum = OrbitNum
CoordStruct.(0).NumBlk   = BlkEnd - BlkBeg + 1
CoordStruct.(0).num_band = !KON.Instr.NBAND
CoordStruct.(0).band_ndx = [2,1,0,3]   ; definition of band order used in MINX

END  ;  FillCoordStruct

;****************************************************************************
PRO InitiateAnimation, MISR_or_AirMISR, OrbitNum, NumCam, CamFiles, $
                       BlockBeg, BlockEnd, Retval
;****************************************************************************

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

MaxMisrSliderOffset    = 100  ; Max MISR offset allowed for slider
MaxAirmisrSliderOffset = 120  ; Max AirMISR offset allowed for slider

;----------------------------------------------------------------------------
; If user canceled out of last dialog, then return and break.
;----------------------------------------------------------------------------

IF (Retval LT 0) THEN BEGIN
   ZOOM_WNDW = -1
   CleanUpMemory, Retval
   Retval = -1
   RETURN
ENDIF

;----------------------------------------------------------------------------
; If not in Process Plume Project option, then allow user to change his/her mind
; about how many blocks to process if > 6 selected.
;----------------------------------------------------------------------------

IF ((BlockEnd - BlockBeg + 1) GT 6) THEN BEGIN
   msg_txt = ['You have requested more than 6 blocks.', 'This may be slow.', $
              'Are you sure you want to do this?']
   result = DIALOG_MESSAGE(msg_txt, /QUESTION, /CENTER)
   IF (STRUPCASE(result) EQ 'NO') THEN BEGIN
      Retval = 99
      RETURN
   ENDIF
ENDIF

;----------------------------------------------------------------------------
; Branch to AirMISR or MISR.
;----------------------------------------------------------------------------

CASE 1 OF

   MISR_or_AirMISR EQ -1 : BEGIN

      ; Processing to restore .sav files occurs in OrbitAnimateOption_gui.
   END

   MISR_or_AirMISR EQ 0 : BEGIN

      ;----------------------------------------------------------------------
      ; MISR data.
      ;----------------------------------------------------------------------

      whichorbit = 0
      CoordStruct.(whichorbit).add_edge = MaxMisrSliderOffset
      CoordStruct.(whichorbit).OrbitNum = OrbitNum[whichorbit]

      IF (N_ELEMENTS(CamFiles) EQ 18) THEN BEGIN
         whichorbit = 1
         CoordStruct.(whichorbit).add_edge = MaxMisrSliderOffset
         CoordStruct.(whichorbit).OrbitNum = OrbitNum[whichorbit]
      ENDIF

   END

   MISR_or_AirMISR EQ 1 : BEGIN

      ;----------------------------------------------------------------------
      ; AirMISR data.
      ;----------------------------------------------------------------------

      whichorbit = 0
      CoordStruct.(whichorbit).add_edge = MaxAirmisrSliderOffset
      CoordStruct.(whichorbit).OrbitNum = -1L ; Used as AirMISR flag.

      whichorbit = 1
      CoordStruct.(whichorbit).add_edge = MaxAirmisrSliderOffset
      CoordStruct.(whichorbit).OrbitNum = -1L ; Used as AirMISR flag.

   END
ENDCASE

;-----------------------------------------------------------------------------
; Display the draw widget to allow selection of processing option and then
; start the animation.
;-----------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

FullAnI, NumCam, CamFiles, CoordStruct, MISR_or_AirMISR, wVeryTopBase, Retval

IF (Retval EQ -1) THEN $
   IF (N_ELEMENTS(wVeryTopBase) GT 0) THEN WIDGET_CONTROL, wVeryTopBase, /DESTROY

ZOOM_WNDW = -1

END  ;  InitiateAnimation

;****************************************************************************
FUNCTION GetFontInfo, Init
;****************************************************************************
; Set a new font to be the active one. If desired font isn't available, use
; the default vector font.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;----------------------------------------------------------------------------
; Initialize the font type to device fonts at program startup.
;----------------------------------------------------------------------------

IF (Init) THEN !P.FONT = 0

;----------------------------------------------------------------------------
; Get the current font info and place it in the return structure.
;----------------------------------------------------------------------------

DEVICE, GET_CURRENT_FONT=current_font

size = ''
face = ''
IF (!P.FONT EQ 0) THEN BEGIN     ; if device fonts were selected
   ipos1 = STRPOS(current_font, '--')
   ipos2 = STRPOS(current_font, '-medium-')
   ipos3 = STRPOS(current_font, '-bold-')
   IF (ipos1 GT 0) THEN BEGIN
      toks = STRSPLIT(STRMID(current_font, ipos1), '-', /EXTRACT)
      IF (toks[0] NE '') THEN size = toks[0]
   ENDIF
   IF (ipos2 GT 0) THEN face = 'medium'
   IF (ipos3 GT 0) THEN face = 'bold'
ENDIF

RETURN, {Type : !P.FONT, Size : size, Face : face}

END  ;  GetFontInfo

;****************************************************************************
PRO SetFontInfo, NewFont
;****************************************************************************
; Set a new font to be the active one. If desired font isn't available, use
; the default vector font.
;----------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;----------------------------------------------------------------------------
; Set error recovery in case we're unsucccessful. In that case, set the font
; to the vector font.
;----------------------------------------------------------------------------

CATCH, error_status
IF (error_status NE 0) THEN BEGIN
   !P.FONT = !KON.FontTyp.FONT_VECTOR
   CATCH, /CANCEL
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Set up the device font type if requested. This may be specific to OS.
;----------------------------------------------------------------------------

!P.FONT = NewFont.Type

dwndw_save = !D.WINDOW

IF (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) THEN BEGIN
   device_str1 = '-adobe-helvetica-'
   device_str2 = '-r-normal--'
   fontstr = device_str1 + NewFont.Face + device_str2 + NewFont.Size + '-*'
   DEVICE, SET_FONT = fontstr
ENDIF

IF (!D.WINDOW NE dwndw_save) THEN BEGIN
   pwin = !D.WINDOW
   SafeWDELETE, pwin, didit
   IF (dwndw_save GE 0) THEN SafeWSET, dwndw_save, didit
ENDIF

;----------------------------------------------------------------------------
; Set up the true type font if requested. Size is in pixels. The 2nd element
; of the size_vals array controls line spacing. This may be specific to OS.
;----------------------------------------------------------------------------

dwndw_save = !D.WINDOW

IF (!P.FONT EQ !KON.FontTyp.FONT_TRUTYP) THEN BEGIN
   truetype_str = 'Helvetica'
   size_vals = [FIX(NewFont.Size), FIX(NewFont.Size) * 1.1]
   DEVICE, SET_FONT=truetype_str, SET_CHARACTER_SIZE=size_vals, /TT_FONT
ENDIF

IF (!D.WINDOW NE dwndw_save) THEN BEGIN
   pwin = !D.WINDOW
   SafeWDELETE, pwin, didit
   IF (dwndw_save GE 0) THEN SafeWSET, dwndw_save, didit
ENDIF

;----------------------------------------------------------------------------
; Clean up.
;----------------------------------------------------------------------------

CATCH, /CANCEL

END  ;  SetFontInfo

;****************************************************************************
;****************************************************************************
PRO MINX
;****************************************************************************
;****************************************************************************
; This is the MINX main routine.
; NOTE - The thread pool is turned on by default to use all CPUs with values:
;        TPOOL_MIN_ELTS = 100000, TPOOL_MAX_ELTS = 0 (unlimited)
;****************************************************************************

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON save_main_parms, MainOption

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;----------------------------------------------------------------------------
; Set once for entire session to enable Equalized Color display.
;----------------------------------------------------------------------------

DEVICE, DECOMPOSED = 1

;rtrn = HEAP_REFCOUNT(/DISABLE)

;----------------------------------------------------------------------------
; Initialize global memory structures.
;----------------------------------------------------------------------------

IF (InitMINXParams() NE 0) THEN RETURN

;----------------------------------------------------------------------------
; Initialize the fonts.
;----------------------------------------------------------------------------

old_font = GetFontInfo(1)

;----------------------------------------------------------------------------
; Loop over main menu options dialog. Each time reinitialize params.
;----------------------------------------------------------------------------

FOR ii=0,9999 DO BEGIN

   ;-------------------------------------------------------------------------
   ; SetFontInfo must have a window active, so it creates one. Then delete it.
   ;-------------------------------------------------------------------------

   SetFontInfo, old_font
   pwin = !D.WINDOW
   SafeWDELETE, pwin, didit

   ;-------------------------------------------------------------------------
   ; Display dialog box that lists main program options.
   ;-------------------------------------------------------------------------

   GetProgramOption_gui, MainOption

   ;-------------------------------------------------------------------------
   ; Depending on the main program option chosen, branch to next dialog box.
   ; If user selects Cancel button from one of these dialogs, then return to
   ; main program option dialog.
   ;-------------------------------------------------------------------------

   SWITCH MainOption OF

    1: BEGIN
       ;---------------------------------------------------------------------
       ; Get swath location parameters and display swath window.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          OrbitLocateOption_gui, PathOrOrbit, PathNum, OrbitNum, BlockBeg, $
                                 BlockEnd, Cancel

          IF (Cancel EQ 1) THEN BEGIN
             pwin = !D.WINDOW
             SafeWDELETE, pwin, didit
             BREAK
          ENDIF

          DisplayOrbitLocation, PathOrOrbit, PathNum, OrbitNum, BlockBeg, $
                                BlockEnd, status
       ENDWHILE
       BREAK
    END

    2: BEGIN
       ;---------------------------------------------------------------------
       ; Find MISR overpasses and show overpass maps.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          OverpassFinder, 0, OrbitList, Cancel

          IF (Cancel LT 0) THEN BEGIN
             pwin = !D.WINDOW
             SafeWDELETE, pwin, didit
             BREAK
          ENDIF
       ENDWHILE
       BREAK
    END

    3: BEGIN
       ;---------------------------------------------------------------------
       ; Get browse parameters and display scroll window.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          OrbitBrowseOption_gui, BrowseFile, PathNum, OrbitNum, BlockBeg, $
                                 BlockEnd, CamName, BandChoice, Resolution, $
                                 Cancel

          IF (Cancel EQ 1) THEN BREAK

          WIDGET_CONTROL, /HOURGLASS
          ShowOrbitBrowseData, BrowseFile, BlockBeg, BlockEnd, BandChoice, $
                               CamName, Resolution, EorT, RorN, status

          IF (status EQ 0) THEN $
             OrbitBrowse, PathNum, OrbitNum, CamName, EorT, RorN
       ENDWHILE
       BREAK
    END

    4: BEGIN
       ;---------------------------------------------------------------------
       ; Display multiple level 1 and level 2 data sets.
       ;---------------------------------------------------------------------

       OrigPath = -1
       WHILE(1) DO BEGIN
          MultiFieldCompare, OrigPath, Cancel
          IF (Cancel EQ 1) THEN BREAK
       ENDWHILE
       BREAK
    END

    5: BEGIN
       ;---------------------------------------------------------------------
       ; Get animate parameters and display draw widget. Ask user to select
       ; a nadir camera file to open. Other camera files will automatically
       ; be selected from 'nearby' directories. Or, the user may select a
       ; .sav file for Restore Session.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          IF (InitMINXParams() NE 0) THEN RETURN

          !SAV.Digitize.PlumeOutDir = !SAV.WorkingDir

redo_animate:
          OrbitAnimateOption_gui, base0, NumCam, CamFiles, CoordStruct, $
                                  OrbitNum, block_beg, block_end, $
                                  MISR_or_AirMISR, Retval

          InitiateAnimation, MISR_or_AirMISR, OrbitNum, NumCam, CamFiles, $
                             block_beg, block_end, Retval

          IF (Retval EQ 99) THEN GOTO, redo_animate
          IF (Retval LT 0) THEN BREAK

          CleanUpMemory, retval
       ENDWHILE
       BREAK
    END

    6: BEGIN
       ;---------------------------------------------------------------------
       ; Update Plume Project preferences.
       ;---------------------------------------------------------------------
       
       PlumeProj_Preferences_gui, Cancel
       
       BREAK
    END
    
    7: BEGIN
       ;---------------------------------------------------------------------
       ; Provide access to production plume processing utility functions.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          DefaultProjDir = !SAV.WorkingDir

          PlumeUtilities_gui, PlumeOption
          IF (PlumeOption EQ -1) THEN BREAK

          IF (PlumeOption EQ  0) THEN ProcessModVolcHotSpots,  DefaultProjDir
          IF (PlumeOption EQ  1) THEN DownloadMOD14Granules,   '', ''
          IF (PlumeOption EQ  2) THEN ProcessModisFirePixels,  DefaultProjDir
          IF (PlumeOption EQ  3) THEN CreateMisrOrderScript,   DefaultProjDir
          IF (PlumeOption EQ  4) THEN CheckMisrFileOrder,      DefaultProjDir
          IF (PlumeOption EQ  5) THEN ConvertJPGsToMP4,        DefaultProjDir
          IF (PlumeOption EQ  6) THEN PlumeReviewRegions_Pass1,DefaultProjDir
          IF (PlumeOption EQ  7) THEN RereviewPlumes_Case1,    DefaultProjDir
          IF (PlumeOption EQ  8) THEN PlumeReviewRegions_Pass2,DefaultProjDir
          IF (PlumeOption EQ  9) THEN RereviewPlumes_Case2,    DefaultProjDir
          IF (PlumeOption EQ 10) THEN VerifyPlumesAfterReview, DefaultProjDir
          IF (PlumeOption EQ 11) THEN PlumeCreateWebFiles,     DefaultProjDir
          IF (PlumeOption EQ 12) THEN CopyFromServerToLocal,   DefaultProjDir
       ENDWHILE
       BREAK
    END

    8: BEGIN
       ;---------------------------------------------------------------------
       ; Start processing plume project data by selecting orbit from list,
       ; by-passing time-consuming dialog boxes.
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN

          IF (InitMINXParams() NE 0) THEN RETURN

          plume_out_dir = !SAV.Digitize.PlumeOutDir
          FillCoordStruct, OrbitNum, BlkBeg, BlkEnd, CamFiles, NumCam, $
                           MISR_or_AirMISR, plume_out_dir, Retval
          IF (Retval LT 0) THEN BREAK
                           
          !SAV.Digitize.PlumeOutDir = plume_out_dir

          InitiateAnimation, MISR_or_AirMISR, OrbitNum, NumCam, CamFiles, $
                             0, 0, Retval

          IF (Retval LT 0) THEN BREAK

          orbitnum = STRTRIM(STRING(CoordStruct.(0).OrbitNum),2)
          beg_blk  = STRTRIM(STRING(CoordStruct.(0).BlkBeg),2)
          end_blk  = STRTRIM(STRING(CoordStruct.(0).BlkEnd),2)
          mssg = ['You just finished processing ', 'orbit ' + orbitnum + $
                  ', blocks ' + beg_blk + ' to ' + end_blk]
          rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

          CleanUpMemory, retval

       ENDWHILE
       BREAK
    END

    9: BEGIN
       ;---------------------------------------------------------------------
       ; Get convert BRF parameters and display scroll window (DEBUG mode only).
       ;---------------------------------------------------------------------

       WHILE(1) DO BEGIN
          ConvertBrfOption_gui, AsSurfaceFile, TerrainFile, PathNum, $
                                OrbitNum, BlockBeg, BlockEnd, ChannelFlags, $
                                RegressSize, DoSmooth, FilterWidth, $
                                DoShowCoef, DoShowBRFs, FileType, Cancel

          IF (Cancel EQ 1) THEN BREAK
          WIDGET_CONTROL, /HOURGLASS
          ConvertBrfData, AsSurfaceFile, TerrainFile, PathNum, OrbitNum, $
                          BlockBeg, BlockEnd, ChannelFlags, RegressSize, $
                          DoSmooth, FilterWidth, DoShowCoef, DoShowBRFs, $
                          FileType, status
       ENDWHILE
       BREAK
    END

    ELSE: BEGIN
       ;---------------------------------------------------------------------
       ; Here if quitting MINX.
       ;---------------------------------------------------------------------

       CleanUpMemory, retval
       RETURN
    END

   ENDSWITCH

ENDFOR

;----------------------------------------------------------------------------
; Restore font information for other apps.
;----------------------------------------------------------------------------

SetFontInfo, old_font

END ; MINX  
