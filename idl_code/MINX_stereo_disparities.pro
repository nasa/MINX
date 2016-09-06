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
PRO FindDisparities, State, pThisRgn, NumPts, ImageCoordsLine, $
                     LandWaterMask, Bands, Offsets, CorrCoeffs, Retval
;***************************************************************************

;***************************************************************************
; Definition of parameters used in determining the smallest effective size
; for comparison windows when doing matching to find disparities (or offsets)
; between feature locations in different camera images.
;***************************************************************************
; Construct a rectangle around the target region with extra room and compute
; fractional pixel along and across disparities using a cross-correlation
; method. Convert along and across disparities into corrected disparities
; parallel and normal to the mean look vector using geometric parameter data.
; Compute disparities every few pixels.
;
; MeanCamTimeFromAn is the average time in seconds it takes for each camera
; to reach the position occupied by the nadir camera to within a percent or
; two. These are used in  estimating the size of comparison windows and in
; computing the cross-track wind from cross-track offset. MeanPer275PixelTime
; is approximately (20.75 / 512.0) sec.
;
; Diagram of coarse-pass comparison window structure, size and position for
; one camera pair at one point:
;
;  --------------------------
;  |                     <----- reference patch ring: constant pixel width =
;  |  ....................  |     half_patch_pix = FLOOR(ref_wndw_size_p1/2)
;  |  .o                o.  |
;  |  .  -------------- <------ edge ring: constant pixel width = 2
;  |  .  |x          x|  .  |     edge_pix = PASS1_STRIDE and is part of
;  |  .  |  ........  |  .  |     comparison window core; if correlation peak
;  |  .  |  .      .  |  .  |     falls in outside pixel, match is rejected
;  |  .  |  .      .  |  .  |    
;  |  .  |  .      . <--------- wind correction ring: along and across
;  |  .  |  .      .  |  .  |     sizes may differ; wind_ring_pix = part
;  |  .  |  .      .  |  .  |     of comparison window core and dependent
;  |  .  |  .      .  |  .  |     on camera and maximum wind speed
;  |  .  |  .      .  |  .  |
;  |  .  |  .    <------------- parallax region: along and across sizes
;  |  .  |  ........  |  .  |     differ; parallax_pix = part of comparison
;  |  .  |x          x|  .  |     window core and dependent on camera,
;  |  .  --------------  .  |     across position in swath and maximum
;  |  .o                o.  |     user-specified height to retrieve
;  |  ....................  |   
;  |                        |   
;  --------------------------     
;
;  | <-- srch_size --> |    size of comparison window in across direction
;
; This structure assumes a decrease in image resolution for the coarse pass
;    of correlation by a factor of PASS1_STRIDE (currently set to 3). It is
;    also dependent on the size of the An camera's square reference
;    patch = ref_wndw_size_p1, which may be different for each camera with
;    which it is paired.
; All coordinates in the coarse pass are initially specified in coarse
;    pixels, i.e. the number of 275 m pixels divided by PASS1_STRIDE. Unless
;    a _275 is appended to variable names, pixel sizes in pass 1 correlation
;    are given in coarse pixels.
; The 'x' positions correspond to 4 possible target pixel coordinates for
;    height/wind retrieval = srch_ref. In the An reference patch, the
;    equivalent position is ref_center, the pixel currently being evaluated.
; The 'o' positions define a rectangle that limits where the reference patch
;    center pixel is positioned during correlation. It must be inside the
;    edge ring.
; The comparison window core is that part of the comparison window in which
;    a valid match can be found: (match coords - target coords) = disparity.
; The comparison window width and height must be odd multiples of
;    PASS1_STRIDE measured in 275 m pixels (the same for 1100 m pixels when
;    low resolution data are being used for correlation).
; The combined widths (and heights) of the 3 core components must be odd
;    multiples of PASS1_STRIDE.
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct
COMMON config, Cancel
                  
;--------------------------------------------------------------------------
; Initialize some values.
;--------------------------------------------------------------------------

SCR_SIZE = [!KON.Misc.ScreenX, !KON.Misc.ScreenY]

POINT_STRIDE = 1   ; process every Nth point in linked lists; set to 1 
                   ; for production; to N for skipping points in debug

DRAW_MAGNIFY = 12  ; magnification of ref and comparison pixel images

DRAW_PATCH = 0     ; set to 1 to show wire frame comparison windows

!SAV.HtWind.CORR_TARGET = 4  ; Required by MINX_image_match to indicate
                             ; we're digitizing a plume/cloud.
     
CorrCoeffs = FLTARR(!KON.Instr.NCAM, NumPts) + !KON.Misc.BADVALUE_REAL

;--------------------------------------------------------------------------
; Establish which cameras will be paired with the An camera in the matching
; process. If validating matcher window positions by auto-correlating
; features in An image, choose only An camera.
;    FIRST_CAM_USE :  0 (Af), 2 (Bf), 4 (Cf) or 6 (Df)  (0 is default)
;    LAST_CAM_USE  :  1 (Aa), 3 (Ba), 5 (Ca) or 7 (Da)  (5 is default)
;--------------------------------------------------------------------------

beg_cam = !SAV.Digitize.FIRST_CAM_USE
end_cam = !SAV.Digitize.LAST_CAM_USE

IF (!SAV.Digitize.AUTO_CORR_AN) THEN BEGIN
   beg_cam = 8
   end_cam = 8
ENDIF 

;--------------------------------------------------------------------------
; Set the resolution to use for pass 2.
;--------------------------------------------------------------------------

Band_Res = !KON.Instr.HI_RES_PIX_SIZE

;--------------------------------------------------------------------------
; Set number of extra rings of pixels around comparison window to ensure
; there is room for the reference window to work.
;--------------------------------------------------------------------------

EdgeRingPix = 2

;--------------------------------------------------------------------------
; Low-res (1100 m) can always be used with all bands. High-res (275 m) can
; always be used with the red-band, but with other bands only if the orbit
; is local-mode (all bands high-res). 
;
; Get An (reference) camera patch size in matcher-size pixels for each
; camera for passes 1 and 2 of correlation. The number of 275 m pixels used
; in each matcher-size pixel is:
;  pass 1 high-res - 4 (average res down and use a larger area)
;  pass 1 low-res  - 4 (use lowest common denominator res and larger area)
;  pass 2 high-res - 1 (use native 275 m resolution for best results)
;  pass 2 low res  - 4 (use lowest common denominator res and smaller area)
;--------------------------------------------------------------------------

ref_size1 = !KON.CorrParam.CORR_SIZE_PASS1[*,!SAV.Digitize.MATCHER_SM_LG-1]
ref_size2 = !KON.CorrParam.CORR_SIZE_PASS2[*,!SAV.Digitize.MATCHER_SM_LG-1]

matcher_pix_p1 = 4
matcher_pix_p2 = 1

;---------------------------------------------------------------------------
; Set band(s) to use for retrieval at all points if not RB_BAND.
; RED_BAND = 0, BLUE_BAND = 1, BOTH_BAND = 2, RB_BAND = 3
;---------------------------------------------------------------------------

IF (!SAV.Digitize.RETRIEVE_BAND_TYPE LE !KON.BandObjTyp.BLUE_BAND) THEN $
   Bands[*] = !SAV.Digitize.RETRIEVE_BAND_TYPE

IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.BOTH_BAND) THEN BEGIN
   IF (!SAV.Digitize.TWO_RETRIEVALS EQ 0) THEN BEGIN
      Bands[*] = (!SAV.Digitize.USE_BAND_NDX EQ !KON.Instr.RED) ? $
                  !KON.BandObjTyp.RED_BAND : !KON.BandObjTyp.BLUE_BAND
   ENDIF ELSE BEGIN
      Bands[*] = !KON.BandObjTyp.BLUE_BAND
   ENDELSE
ENDIF

;***************************************************************************
; Loop over the points on a direction line or the points in a region of
; interest for which heights (and winds) are to be computed.
;***************************************************************************

FOR ipts=0,NumPts-1,POINT_STRIDE DO BEGIN

   ;------------------------------------------------------------------------
   ; Set band to use for retrieval at this point. The only situation where
   ; this varies for points in a region is if the user chose to use the red
   ; band over water and the blue band over land.
   ; Band indexes are: Red = 0, Blue = 2.
   ; Save the band used for this point's retrieval to be added later to the
   ; linked list point structure where Red = 0 or Blue = 1.
   ;------------------------------------------------------------------------

   USE_BAND = !SAV.Digitize.USE_BAND_NDX

   IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.RB_BAND) THEN BEGIN
      IF (LandWaterMask[ipts] EQ 0 OR LandWaterMask[ipts] EQ 5 OR $
          LandWaterMask[ipts] EQ 6) THEN BEGIN
         USE_BAND = !KON.Instr.RED
         band_for_lw_mask = !KON.BandObjTyp.RED_BAND
      ENDIF ELSE BEGIN
         USE_BAND = !KON.Instr.BLU
         band_for_lw_mask = !KON.BandObjTyp.BLUE_BAND      
      ENDELSE
      Bands[ipts] = band_for_lw_mask
   ENDIF
 
   ;------------------------------------------------------------------------
   ; Reinitialize the structures for storing and passing reference and
   ; comparison window parameters.
   ;------------------------------------------------------------------------

   REF = { target_275    : [0, 0],     $
           target_1100   : [0, 0],     $
           size_275_p1   : [0, 0],     $
           size_1100_p1  : [0, 0],     $
           size_275_p2   : [0, 0],     $
           size_1100_p2  : [0, 0],     $
           beg_275       : [0, 0],     $
           end_275       : [0, 0],     $
           beg_1100      : [0, 0],     $
           end_1100      : [0, 0],     $
           beg_275_adj   : [0.0, 0.0], $
           beg_1100_adj  : [0.0, 0.0]  $
   }

   CMP = { target_275    : [0, 0],     $
           target_1100   : [0, 0],     $
           size_275_p1   : [0, 0],     $
           size_1100_p1  : [0, 0],     $
           size_275_p2   : [0, 0],     $
           size_1100_p2  : [0, 0],     $
           beg_275       : [0, 0],     $
           end_275       : [0, 0],     $
           beg_1100      : [0, 0],     $
           end_1100      : [0, 0],     $
           totarget_275  : [0.0, 0.0], $
           totarget_1100 : [0.0, 0.0]  $
   }

   REF_SRCH_WNDWS = [0, 0, 0, 0]

   ;------------------------------------------------------------------------
   ; Set the offsets (in 275 m pix coords) for the nadir camera to 0.
   ;------------------------------------------------------------------------

   Offsets[0:1, !KON.Instr.AN, ipts] = 0.0

   ;------------------------------------------------------------------------
   ; Get the coordinates of next point in the An camera image to match.
   ; This becomes the center point of the reference patch. The along (y)
   ; coordinate is transformed so the origin is at the TOP of the image.
   ;------------------------------------------------------------------------

   xy_275    = FLOAT(ImageCoordsLine[0:1,ipts])
   xy_275[1] = State.sizey - xy_275[1] - 1

   REF.target_275  = FLOOR(xy_275 / matcher_pix_p1) * matcher_pix_p1
   REF.target_1100 = xy_275 / matcher_pix_p1

   ;************************************************************************
   ; Loop over the comparison cameras (An is the reference camera).
   ;************************************************************************

   FOR icam = beg_cam, end_cam DO BEGIN

      status_code = -1
      success = 0
   
      ;---------------------------------------------------------------------
      ; Initialize some parameters. Especially get the sizes of reference
      ; window for the 2 passes of correlation.
      ; CAM_ORDER = [ 3,   5,   2,   6,   1,   7,   0,   8,   4]
      ;---------------------------------------------------------------------

      cam_num = !KON.Instr.CAM_ORDER[icam]

      REF.size_1100_p1 = [ref_size1[cam_num], ref_size1[cam_num]]
      REF.size_1100_p2 = [ref_size2[cam_num], ref_size2[cam_num]]

      ;---------------------------------------------------------------------
      ; If this is the nadir camera, skip it, unless testing.
      ;---------------------------------------------------------------------

      IF (cam_num EQ !KON.Instr.AN AND ~!SAV.Digitize.AUTO_CORR_AN) THEN $
         CONTINUE

      ;---------------------------------------------------------------------
      ; If a fatal error occurs, detect it, skip it and continue.
      ;---------------------------------------------------------------------

      CATCH, error_status
      IF (error_status NE 0) THEN BEGIN
         CATCH, /CANCEL
         CONTINUE
      ENDIF

      ;*********************************************************************
      ; PASS 1 - Compute reference window parameters. Both dimensions have
      ;          the same odd number of pixels.
      ;*********************************************************************

      ;---------------------------------------------------------------------
      ; Compute the reference window start and size coordinates in 275 m
      ; pixels. This window is always a square patch extracted from the An
      ; camera image. It must be smaller than the comparison window that is
      ; extracted from one of the other camera images.
      ;---------------------------------------------------------------------

      REF.size_275_p1 = REF.size_1100_p1 * matcher_pix_p1

      REF.beg_275  = FIX(REF.target_275) - REF.size_275_p1 / $
                        (2 * matcher_pix_p1) * matcher_pix_p1
      REF.end_275  = REF.beg_275  + REF.size_275_p1  - 1
      REF.beg_1100 = REF.beg_275 / matcher_pix_p1
      REF.end_1100 = REF.beg_1100 + REF.size_1100_p1 - 1

      REF.beg_275_adj  = FIX(xy_275 - REF.beg_275) MOD 4
      REF.beg_1100_adj = REF.beg_275_adj / matcher_pix_p1

      ;---------------------------------------------------------------------
      ; The following adjustments for reference windows near top & bottom
      ; edges of the MISR data window are needed only in along direction,
      ; because there are no-data zones on swath edges in across direction.
      ;---------------------------------------------------------------------

      IF (REF.beg_275[1] LT 0) THEN BEGIN
         REF.beg_275[1]  = 0
         REF.end_275[1]  = REF.beg_275[1]  + REF.size_275_p1[1] - 1
         REF.beg_1100[1] = 0
         REF.end_1100[1] = REF.beg_1100[1] + REF.size_1100_p1[1] - 1
         REF.beg_275_adj[1]  = FIX(xy_275[1] - REF.beg_275[1]) MOD 4
         REF.beg_1100_adj[1] = REF.beg_275_adj[1] / matcher_pix_p1
      ENDIF

      IF (REF.end_275[1] GT (State.sizey - 1)) THEN BEGIN
         REF.end_275[1]  = State.sizey - 1
         REF.beg_275[1]  = REF.end_275[1] - REF.size_275_p1[1] + 1
         REF.end_1100[1] = State.sizey / matcher_pix_p1 - 1
         REF.beg_1100[1] = REF.end_1100[1] - REF.size_1100_p1[1] + 1
         REF.beg_275_adj[1]  = FIX(xy_275[1] - REF.beg_275[1]) MOD 4
         REF.beg_1100_adj[1] = REF.beg_275_adj[1] / matcher_pix_p1
      ENDIF

      ;*********************************************************************
      ; PASS 1 - Compute common comparison window parameters.
      ;*********************************************************************

      ;---------------------------------------------------------------------
      ; Compute the width in matcher pixels of the reference patch ring that
      ; accommodates half the reference patch size.
      ;---------------------------------------------------------------------

      half_patch_pix = REF.size_1100_p1 / 2

      ;---------------------------------------------------------------------
      ; Compute the approximate maximum number of matcher pixels the wind
      ; can offset the feature for this camera given the camera and the
      ; maximum wind speed specified by the user. If this region does not
      ; require a wind-correction, set this to 0.
      ;---------------------------------------------------------------------

      IF ((*pThisRgn).ObjType[2] GE $
          !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN
         Sec_fromAn = ABS(!KON.Instr.MeanCamTimeFromAn[cam_num])
         max_wind_offset_meter = Sec_fromAn * !SAV.Digitize.MAX_WIND
         wind_ring_pix_f = max_wind_offset_meter / 1000.0 / $
                           !KON.Instr.HI_RES_PIX_SIZE
      ENDIF ELSE BEGIN
         wind_ring_pix_f = 0.0
      ENDELSE

      core_pix = [0, 0]

      ;******************************************************************
      ; PASS 1 - Compute comparison window parameters for across direction.
      ;******************************************************************

      ;---------------------------------------------------------------------
      ; Compute width in matcher pixels of center portion of comparison
      ; window that accommodates maximum offset due to parallax, given
      ; camera geometry and maximum aerosol height specified by the user.
      ; The sign changes from left (+) to right (-) of swath center.
      ;---------------------------------------------------------------------

      proj_offset = (1024.0 - REF.target_275[0]) * !KON.Instr.HI_RES_PIX_SIZE
      parallax_pix_f = !SAV.Digitize.MAX_HGHT / !KON.Instr.HI_RES_PIX_SIZE * $
                       proj_offset / !KON.Instr.MISR_altitude

      ;---------------------------------------------------------------------
      ; Compute the total width of the wind and parallax components of the
      ; comparison window. This must be an odd number of matcher pixels.
      ; This ensures that the total comparison window width is odd.
      ;---------------------------------------------------------------------

      wind_parallax_pix = wind_ring_pix_f * 2.0 + ABS(parallax_pix_f)
      core_pix[0] = CEIL(wind_parallax_pix / matcher_pix_p1) > 1
      core_pix[0] += ((core_pix[0] MOD 2) EQ 0) ? 1 : 0

      ;---------------------------------------------------------------------
      ; CMP.totarget_1100[0] is the number of matcher pixels across from
      ;    the origin of the comparison window to the point comparable to
      ;    the center of the reference window.
      ;---------------------------------------------------------------------

      CMP.totarget_1100[0] = half_patch_pix[0] + EdgeRingPix + $
                             ((parallax_pix_f GT 0.0) ? (core_pix[0] - 1) : 0)
      CMP.totarget_275[0] = CMP.totarget_1100[0] * 4 + REF.beg_275_adj[0]

      ;*********************************************************************
      ; PASS 1 - Compute comparison window parameters for along direction.
      ;*********************************************************************

      ;---------------------------------------------------------------------
      ; Compute the height in matcher pixels of the center portion of
      ; window that accommodates the maximum offset due to parallax, given
      ; camera geometry and maximum aerosol height specified by the user.
      ; The sign changes going from forward to aft cameras.
      ;---------------------------------------------------------------------

      parallax_coeff = TAN(!KON.Instr.CamViewAngles[cam_num] / $
                           !KON.Misc.DoubleRadeg)
      parallax_pix_f = !SAV.Digitize.MAX_HGHT / !KON.Instr.HI_RES_PIX_SIZE * $
                       parallax_coeff

      ;---------------------------------------------------------------------
      ; Compute the total height of the wind and parallax components of
      ; the comparison window. This width must be an odd number of matcher
      ; pixels. This ensures that the total comparison window width is odd.
      ;---------------------------------------------------------------------

      wind_parallax_pix = wind_ring_pix_f * 2.0 + ABS(parallax_pix_f)
      core_pix[1] = CEIL(wind_parallax_pix / matcher_pix_p1) > 1
      core_pix[1] += ((core_pix[1] MOD 2) EQ 0) ? 1 : 0

      ;---------------------------------------------------------------------
      ; CMP.totarget_1100[1] is the number of matcher pixels along from the
      ;    origin of the comparison window to the point comparable to the
      ;    center of the reference window.
      ;---------------------------------------------------------------------

      CMP.totarget_1100[1] = half_patch_pix[1] + EdgeRingPix + $
                            ((parallax_pix_f GT 0.0) ? (core_pix[1] - 1) : 0)
      CMP.totarget_275[1] = CMP.totarget_1100[1] * 4 + REF.beg_275_adj[1]

      ;---------------------------------------------------------------------
      ; Compute the comparison window parameters that have the same form in
      ; across and along directions from those above:
      ; CMP.size_..._p1 are across and along sizes of window in pixels.
      ; CMP.beg_... are pixel indexes of the start of the comparison window
      ;     from the left and top origin of the entire MINX image.
      ; CMP.end_... are pixel indexes of the end of the comparison window.
      ;---------------------------------------------------------------------

      CMP.size_1100_p1 = (half_patch_pix + EdgeRingPix) * 2 + core_pix
      CMP.size_275_p1  = CMP.size_1100_p1 * matcher_pix_p1

      CMP.beg_1100 = FLOOR(REF.target_1100 - CMP.totarget_1100)
      CMP.end_1100 = CMP.beg_1100 + CMP.size_1100_p1 - 1
      CMP.beg_275  = CMP.beg_1100 * matcher_pix_p1
      CMP.end_275  = CMP.beg_275  + CMP.size_275_p1 - 1

      ;---------------------------------------------------------------------
      ; The following adjustments for comparison windows near top & bottom
      ; edges of the MISR data window are needed only in along direction,
      ; because there are no-data zones on swath edges in across direction.
      ;---------------------------------------------------------------------

      IF (CMP.beg_275[1] LT 0) THEN BEGIN
         beg_shft = CMP.beg_275[1]
         CMP.beg_275[1]  = 0
         CMP.end_275[1]  = CMP.beg_275[1]  + CMP.size_275_p1[1] - 1
         CMP.beg_1100[1] = 0
         CMP.end_1100[1] = CMP.beg_1100[1] + CMP.size_1100_p1[1] - 1
         CMP.totarget_1100[1] += beg_shft / matcher_pix_p1
         CMP.totarget_275[1] = CMP.totarget_1100[1] * matcher_pix_p1 + $
                               REF.beg_275_adj[1]
      ENDIF

      IF (CMP.end_275[1] GT (State.sizey - 1)) THEN BEGIN
         end_shft = CMP.end_275[1] - State.sizey + 1
         CMP.end_275[1]  = State.sizey - 1
         CMP.beg_275[1]  = CMP.end_275[1] - CMP.size_275_p1[1] + 1
         CMP.end_1100[1] = State.sizey / matcher_pix_p1 - 1
         CMP.beg_1100[1] = CMP.end_1100[1] - CMP.size_1100_p1[1] + 1
         CMP.totarget_1100[1] += end_shft / matcher_pix_p1
         CMP.totarget_275[1] = CMP.totarget_1100[1] * matcher_pix_p1 + $
                               REF.beg_275_adj[1]
      ENDIF

      ;*********************************************************************
      ; PASS 1 - Get image data and change to lower resolution as needed. 
      ;*********************************************************************

      ;---------------------------------------------------------------------
      ; Extract the square reference window from An camera's raw data array
      ; for the first matcher pass. x1, x2, y1, y2 must each be divisible
      ; by 4.
      ;---------------------------------------------------------------------

      xyr1 = REF.beg_275
      xyr2 = REF.end_275

      ref_wndw = GetRawImageAllAt1100(xyr1[0], xyr2[0], xyr1[1], xyr2[1], $
                                      USE_BAND, !KON.Instr.AN)

      IF ((xyr1[0] NE REF.beg_275[0]) OR (xyr2[0] NE REF.end_275[0]) OR $
          (xyr1[1] NE REF.beg_275[1]) OR (xyr2[1] NE REF.end_275[1])) THEN $
         GOTO, cleanup

      ndxbad = WHERE(ref_wndw LE 0.0, numbad)
      IF (numbad GT 0) THEN ref_wndw[ndxbad] = !VALUES.F_NAN

      ;---------------------------------------------------------------------
      ; Extract the larger, rectangular comparison window from current
      ; non-An camera's raw data array for the first matcher pass.
      ;---------------------------------------------------------------------

      xyc1 = CMP.beg_275
      xyc2 = CMP.end_275

      cmp_wndw = GetRawImageAllAt1100(xyc1[0], xyc2[0], xyc1[1], xyc2[1], $
                                      USE_BAND, cam_num)

      IF ((xyc1[0] NE CMP.beg_275[0]) OR (xyc2[0] NE CMP.end_275[0]) OR $
          (xyc1[1] NE CMP.beg_275[1]) OR (xyc2[1] NE CMP.end_275[1])) THEN $
         GOTO, cleanup

      ndxbad = WHERE(cmp_wndw LE 0.0, numbad)
      IF (numbad GT 0) THEN cmp_wndw[ndxbad] = !VALUES.F_NAN

      ;---------------------------------------------------------------------
      ; If selected by user, display magnified images of the reference and
      ; comparison camera comparison windows. Draw red '+'s at locations
      ; of the target pixel in each window.
      ;---------------------------------------------------------------------

      IF (!SAV.Digitize.SHOW_REF_CMP) THEN BEGIN
         ref_position = [(xy_275[0] - REF.beg_275[0] + 0.5) / matcher_pix_p1, $
                         (REF.end_275[1] - xy_275[1] + 0.5) / matcher_pix_p1]
         cmp_position = [(xy_275[0] - CMP.beg_275[0] + 0.5) / matcher_pix_p1, $
                         (CMP.end_275[1] - xy_275[1] + 0.5) / matcher_pix_p1]
         DrawRefSrchImages, 1, cam_num, ref_wndw, cmp_wndw, ref_position, $
                            cmp_position, SCR_SIZE, REF_SRCH_WNDWS, $
                            DRAW_MAGNIFY, xysiz
         cmp_pos_save = cmp_position
      ENDIF

      ;*********************************************************************
      ; PASS 1 - Apply cross-correlation algorithm to find preliminary
      ; along and across disparities (offsets) in whole pixels at coarse
      ; resolution on the comparison grid. This pass is designed to be fast
      ; to search over a large comparison space. Near-nadir cameras may not
      ; need this optimization but are incldued.
      ;*********************************************************************

      offset_pair = [!KON.Misc.BADVALUE_REAL, !KON.Misc.BADVALUE_REAL]
      save_offset = [!KON.Misc.BADVALUE_REAL, !KON.Misc.BADVALUE_REAL]

      corr_matrix = FLTARR(CMP.size_1100_p1[0], CMP.size_1100_p1[1]) + $
                          !KON.Misc.BADVALUE_REAL

      CorrelatePlume, icam, corr_wndw1, 1, ref_wndw, cmp_wndw, $
                      CMP.totarget_1100, REF.size_1100_p1, CMP.size_1100_p1, $
                      !SAV.Digitize.DRAW_CORR_MTRX, corr_matrix, $
                      offset_pair, max_corr1, corr_failed1, status_code

      IF (corr_failed1 NE 0 AND !SAV.Digitize.DRAW_CORR_MTRX EQ 1) THEN BEGIN
         ival = DIALOG_MESSAGE('Press Yes to continue, No to cancel plots.', $
                               /QUESTION, /CENTER)
         IF (STRUPCASE(ival) EQ 'NO') THEN !SAV.Digitize.DRAW_CORR_MTRX = 0
         SafeWDELETE, corr_wndw1, didit
      ENDIF

      IF (status_code NE 0) THEN GOTO, cleanup

      ;---------------------------------------------------------------------
      ; Show debug data.
      ;---------------------------------------------------------------------

      IF (!SAV.Digitize.SHOW_REF_CMP) THEN BEGIN

         ;------------------------------------------------------------------
         ; Post a green 'X' on the comparison camera image (the larger
         ; comparison window) at the location that correlation results
         ; indicate is the feature point corresponding to the center point
         ; in the reference camera image.
         ;------------------------------------------------------------------

         IF (~ corr_failed1) THEN BEGIN
            SafeWSET, REF_SRCH_WNDWS[1], didit
            PLOT, ([(cmp_pos_save[0] + offset_pair[0]) * DRAW_MAGNIFY]), $
                  ([(cmp_pos_save[1] - offset_pair[1]) * DRAW_MAGNIFY]), $
                  POSITION=[0,0,xysiz[0]-1,xysiz[1]-1], $
                  COLOR=!KON.Colors.ColorVals[1], PSYM=7, $
                  XRANGE=[0,xysiz[0]-1], YRANGE=[0,xysiz[1]-1], $
                  SYMSIZE=1.2, XSTYLE=5, YSTYLE=5, /NOERASE
         ENDIF

         ;------------------------------------------------------------------
         ; Show a message box listing critical parameters if the option
         ; is selected in digitizing dialog box. To use, set a breakpoint
         ; on the last line before the ENDIF before running, then run, and
         ; when breakpoint is reached, make an image of the message box
         ; before killing it.
         ;------------------------------------------------------------------

;         RefCmpDialog, 1, REF, CMP, cam_num, matcher_pix_p1, $
;                       half_patch_pix[0], EdgeRingPix, core_pix, $
;                       offset_pair, cmp_pos_save, rtrn
;
;         IF (rtrn EQ 'No') THEN !SAV.Digitize.SHOW_REF_CMP = 0
      ENDIF

      ;---------------------------------------------------------------------
      ; Save the offsets (disparities) from pass 1 in 275 m size pixels and
      ; clean up after pass 1.
      ;---------------------------------------------------------------------

      save_offset = FIX(offset_pair * matcher_pix_p1)

      ref_wndw = 0
      cmp_wndw = 0
      status_code = -1

      ;---------------------------------------------------------------------
      ; If the first pass didn't succeed, try the next point.
      ;---------------------------------------------------------------------

      IF (corr_failed1 NE 0) THEN GOTO, cleanup

      ;*********************************************************************
      ; PASS 2 - Set up window parameters and get image data.
      ;*********************************************************************

      ;---------------------------------------------------------------------
      ; If the first pass succeeded, use those results to set up matcher
      ; window parameters for the second pass. If doing lo-res retrieval,
      ; e.g. with blue band, the REF.target_275 input coords are guaranteed
      ; to be divisible by matcher_pix_p2, so REF.beg_275 will be on a
      ; lo-res pix boundary.
      ;---------------------------------------------------------------------

      REF.size_275_p2 = REF.size_1100_p2 * matcher_pix_p2

      REF.beg_275 = FIX(xy_275 - REF.size_275_p2 / 2) * $
                    matcher_pix_p2 / matcher_pix_p2
      REF.end_275 = REF.beg_275 + REF.size_275_p2 - 1

      REF.size_1100_p2 = REF.size_275_p2 / matcher_pix_p2
      REF.beg_1100 = REF.beg_275 / matcher_pix_p2
      REF.end_1100 = REF.end_275 / matcher_pix_p2

      xyr1 = REF.beg_275 > [0, 0]
      xyr2 = REF.end_275 < [(State.sizex-1), (State.sizey-1)]

      ;---------------------------------------------------------------------
      ; Compute a new center for the comparison window that should closely
      ; match features in the reference window.
      ;---------------------------------------------------------------------
 
      CMP.size_275_p2   = REF.size_275_p2 * 2 + 1
      CMP.target_275    = ROUND(xy_275 + save_offset)
      CMP.totarget_275  = CMP.size_275_p2 / 2
      CMP.size_1100_p2  = CMP.size_275_p2  / matcher_pix_p2
      CMP.beg_275       = FIX(CMP.target_275 - CMP.totarget_275) / $
                              matcher_pix_p2 * matcher_pix_p2
      CMP.end_275       = CMP.beg_275 + CMP.size_275_p2 - 1
      CMP.totarget_1100 = CMP.totarget_275 / matcher_pix_p2

      ;---------------------------------------------------------------------
      ; Extract the square reference window and the ALSO square comparison
      ; window from the raw data arrays for second, fine matcher pass. If
      ; the band to be retrieved is low-res (1100 m pixels), then regress
      ; the band against the red-band from the same camera, being careful
      ; to specify coordinates that are an even multiple of 4 so the low-
      ; res pixels are selected correctly.
      ;---------------------------------------------------------------------

      xyc1 = CMP.beg_275 > [0, 0]
      xyc2 = CMP.end_275 < [(State.sizex-1), (State.sizey-1)]

      ref_wndw = GetRawImage(xyr1[0], xyr2[0], xyr1[1], xyr2[1], $
                             USE_BAND, !KON.Instr.AN, Band_Res, $
                             !KON.Misc.INTERP_TYPE_SAMP)

      IF ((SIZE(*!VAR.RawImages[USE_BAND,cam_num]))[1] GE $
          !KON.Instr.HI_RES_PIX_CROSS) THEN BEGIN

         cmp_wndw = GetRawImage(xyc1[0], xyc2[0], xyc1[1], xyc2[1], $
                                USE_BAND, cam_num, Band_Res, $
                                !KON.Misc.INTERP_TYPE_SAMP)
         FillMissingPixelsInBRFs, cmp_wndw, Retval
         
      ENDIF ELSE BEGIN
         xyc1[0] = FLOOR(xyc1[0] / 4.) * 4
         xyc2[0] = CEIL (xyc2[0] / 4.) * 4 - 1
         IF (xyc2[0] LT CMP.end_275[0]) THEN xyc2[0] += 4
         xsize = xyc2[0] - xyc1[0] + 1

         xyc1[1] = FLOOR(xyc1[1] / 4.) * 4
         xyc2[1] = CEIL (xyc2[1] / 4.) * 4 - 1
         IF (xyc2[1] LT CMP.end_275[1]) THEN xyc2[1] += 4
         ysize = xyc2[1] - xyc1[1] + 1

         RegressBandAgainstRed, xyc1[0], xyc2[0], xyc1[1], xyc2[1], $
                                USE_BAND, cam_num, xsize, ysize, 0, 1, $
                                temp_cmp_wndw, retval

         x_beg = CMP.beg_275[0] - xyc1[0]
         x_end = x_beg + CMP.size_275_p2[0] - 1
         y_beg = CMP.beg_275[1] - xyc1[1]
         y_end = y_beg + CMP.size_275_p2[1] - 1

         cmp_wndw = temp_cmp_wndw[x_beg:x_end, y_beg:y_end]
         temp_cmp_wndw = 0
      ENDELSE

      ndxbad = WHERE(ref_wndw LE 0.0, numbad)
      IF (numbad GT 0) THEN ref_wndw[ndxbad] = !VALUES.F_NAN

      ndxbad = WHERE(cmp_wndw LE 0.0, numbad)
      IF (numbad GT 0) THEN cmp_wndw[ndxbad] = !VALUES.F_NAN

      ndxbad = 0
 
      ;*********************************************************************
      ; PASS 2 - Display magnified images of the reference and comparison
      ; camera windows.
      ;*********************************************************************

      IF (!SAV.Digitize.SHOW_REF_CMP) THEN BEGIN

         ref_position = [(xy_275[0] - REF.beg_275[0] + 0.5) / matcher_pix_p2, $
                         (REF.end_275[1] - xy_275[1] + 0.5) / matcher_pix_p2]
         cmp_position = [(CMP.target_275[0] - CMP.beg_275[0] + 0.5) / matcher_pix_p2, $
                         (CMP.end_275[1] - CMP.target_275[1] + 0.5) / matcher_pix_p2]
         DrawRefSrchImages, 2, cam_num, ref_wndw, cmp_wndw, ref_position, $
                            cmp_position, SCR_SIZE, REF_SRCH_WNDWS, $
                            DRAW_MAGNIFY, xysiz
         cmp_pos_save = cmp_position
      ENDIF

      ;*********************************************************************
      ; PASS 2 - Apply cross-correlation algorithm to find final along and
      ; across disparities (offsets) in fractional pixels at fine resolution
      ; on the comparison grid.
      ;*********************************************************************

      offset_pair = [!KON.Misc.BADVALUE_REAL, !KON.Misc.BADVALUE_REAL]

      corr_matrix = FLTARR(CMP.size_1100_p2[0], CMP.size_1100_p2[1]) + $
                           !KON.Misc.BADVALUE_REAL

      CorrelatePlume, icam, corr_wndw2, 2, ref_wndw, cmp_wndw, $
                      CMP.totarget_1100, REF.size_1100_p2, CMP.size_1100_p2, $
                      !SAV.Digitize.DRAW_CORR_MTRX, corr_matrix, $
                      offset_pair, max_corr2, corr_failed2, status_code

      IF (status_code EQ 0) THEN CorrCoeffs[cam_num,ipts] = max_corr2

      IF ((ABS(offset_pair[0]) LT 999.) AND $
          (ABS(offset_pair[1]) LT 999.)) THEN success = 1

      IF (!SAV.Digitize.DRAW_CORR_MTRX EQ 1) THEN BEGIN
         ival = DIALOG_MESSAGE('Press Yes to continue, No to cancel plots.', $
                               /QUESTION, /CENTER)
         IF (STRUPCASE(ival) EQ 'NO') THEN !SAV.Digitize.DRAW_CORR_MTRX = 0
         SafeWDELETE, corr_wndw1, didit
         SafeWDELETE, corr_wndw2, didit
      ENDIF

      ;---------------------------------------------------------------------
      ; Cancel the error catcher for camera matching.
      ;---------------------------------------------------------------------

cleanup:
      CATCH, /CANCEL

      ;---------------------------------------------------------------------
      ; Save the disparities in an array to return. Offsets are set to
      ; positive if the match point in the comparison camera is to the right
      ; and above the point in reference camera. save_offset was converted
      ; to 275 m pixels above.
      ;---------------------------------------------------------------------

      IF (status_code GE 0 AND success GT 0) THEN BEGIN
         orig_offsets = offset_pair
         offset_pair = offset_pair * matcher_pix_p2 + save_offset
         offset_pair[1] *= -1.0
         Offsets[0:1,cam_num,ipts] = offset_pair
      ENDIF ELSE BEGIN
         corr_failed2 = 1
         Offsets[0:1,cam_num,ipts] = !KON.Misc.BADVALUE_REAL
      ENDELSE

      ;---------------------------------------------------------------------
      ; Pass 2 - Post a green 'X' on the comparison camera image (larger
      ; comparison window) the location that correlation results indicate is
      ; the feature point corresponding to the center point in the reference
      ; camera image.
      ;---------------------------------------------------------------------

      IF (!SAV.Digitize.SHOW_REF_CMP) THEN BEGIN

         IF (~ corr_failed2) THEN BEGIN
            SafeWSET, REF_SRCH_WNDWS[3], didit
            PLOT, [(cmp_pos_save[0] + orig_offsets[0]) * DRAW_MAGNIFY], $
                  [(cmp_pos_save[1] - orig_offsets[1]) * DRAW_MAGNIFY], $
                  POSITION=[0,0,xysiz[0]-1,xysiz[1]-1], /NOERASE, $
                  COLOR=!KON.Colors.ColorVals[1], PSYM=7, SYMSIZE=1.2, $
                  XRANGE=[0,xysiz[0]-1], YRANGE=[0,xysiz[1]-1], $
                  XSTYLE=5, YSTYLE=5
         ENDIF

         ;------------------------------------------------------------------
         ; Show a message box listing critical parameters if the option is
         ; selected in digitizing dialog box (see previous call).
         ;------------------------------------------------------------------

         RefCmpDialog, 2, REF, CMP, cam_num, matcher_pix_p2, $
                       half_patch_pix[0], EdgeRingPix, core_pix, $
                       offset_pair, cmp_pos_save, rtrn

         IF (rtrn EQ 'No') THEN !SAV.Digitize.SHOW_REF_CMP = 0

         ;------------------------------------------------------------------
         ; Delete the debug windows if they exist.
         ;------------------------------------------------------------------

         FOR iwndw=0,3 DO BEGIN
            SafeWDELETE, REF_SRCH_WNDWS[iwndw], didit
            REF_SRCH_WNDWS[iwndw] = -1
         ENDFOR

      ENDIF

      ;---------------------------------------------------------------------
      ; Clean up after pass 2.
      ;---------------------------------------------------------------------

      ref_wndw = 0  
      cmp_wndw = 0
      temp_wndw = 0
      corr_matrix = 0
      offset_pair = 0

   ENDFOR

   REF = 0
   CMP = 0

ENDFOR

END  ;  FindDisparities

;***************************************************************************
PRO DrawRefSrchImages, pass, cam_num, ref_wndw, cmp_wndw, ref_pos, cmp_pos, $
                       SCR_SIZE, REF_SRCH_WNDWS, DRAW_MAGNIFY, xysiz
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

maxval1 = MAX(ref_wndw, MIN=minval1, /NAN)
maxval2 = MAX(cmp_wndw, MIN=minval2, /NAN)
minval  = minval1 < minval2
maxval  = maxval1 > maxval2

;--------------------------------------------------------------------------
; Display the extracted data for the reference camera in the small patch.
; Draw a red '+' to mark the position of center pixel where correlation is
; to occur.
;--------------------------------------------------------------------------

xysiz = (SIZE(ref_wndw))[1:2] * DRAW_MAGNIFY

posx = SCR_SIZE[0]/2 - 300 - xysiz[0]
posy = SCR_SIZE[1]/2 + xysiz[1] * ((pass EQ 1) ? 1 : (-1))

WINDOW, XSIZE=xysiz[0], YSIZE=xysiz[1], XPOS=posx, YPOS=posy, $
        RETAIN=2, /FREE
REF_SRCH_WNDWS[(pass EQ 1) ? 0 : 2] = !D.WINDOW

img = REVERSE(REBIN(ref_wndw, xysiz[0], xysiz[1], /SAMPLE), 2)
img[0,0] = minval
img[0,1] = maxval
TV, BYTSCL(img, /NAN, MIN=minval, MAX=maxval)

xypt = ref_pos * DRAW_MAGNIFY

PLOT, [xypt[0]], [xypt[1]], POSITION=[0,0,xysiz[0]-1,xysiz[1]-1], PSYM=1, $
      XRANGE=[0,xysiz[0]-1], YRANGE=[0,xysiz[1]-1], SYMSIZE=1.2, $
      COLOR=!KON.Colors.ColorVals[0], XSTYLE=5, YSTYLE=5, /NOERASE
XYOUTS, xysiz[0]/2, xysiz[1]-10, 'Pass ' + STRING(FORMAT='(I1)',pass) + $
        ': An', ALIGNMENT=0.5, COLOR=!KON.Colors.ColorVals[4]

;--------------------------------------------------------------------------
; Display the extracted data for the comparison camera in larger window.
; Draw a red '+' to mark the position of the pixel at the same coordinates
; as in the reference window.
;--------------------------------------------------------------------------

xysiz = (SIZE(cmp_wndw))[1:2] * DRAW_MAGNIFY

posx = SCR_SIZE[0]/2 + 300
posy = SCR_SIZE[1]/2 + xysiz[1] * ((pass EQ 1) ? 1 : (-1))

WINDOW, XSIZE=xysiz[0], YSIZE=xysiz[1], XPOS=posx, YPOS=posy, $
        RETAIN=2, /FREE
REF_SRCH_WNDWS[(pass EQ 1) ? 1 : 3] = !D.WINDOW

img = REVERSE(REBIN(cmp_wndw, xysiz[0], xysiz[1], /SAMPLE), 2)
img[0,0] = minval
img[0,1] = maxval
TV, BYTSCL(img, /NAN, MIN=minval, MAX=maxval)

xypt = cmp_pos * DRAW_MAGNIFY

PLOT, [xypt[0]], [xypt[1]], POSITION=[0,0,xysiz[0]-1,xysiz[1]-1], PSYM=1, $
      XRANGE=[0,xysiz[0]-1], YRANGE=[0,xysiz[1]-1], SYMSIZE=1.2, $
      COLOR=!KON.Colors.ColorVals[0], XSTYLE=5, YSTYLE=5, /NOERASE
XYOUTS, xysiz[0]/2, xysiz[1]-10, 'Pass ' + STRING(FORMAT='(I1)',pass) + $
        ': ' + !KON.Instr.camera_names[cam_num+1], ALIGNMENT=0.5, $
        COLOR=!KON.Colors.ColorVals[4]
XYOUTS, xysiz[0]/2, xysiz[1]-20, 'Before correlation', ALIGNMENT=0.5, $
        COLOR=!KON.Colors.ColorVals[0]
XYOUTS, xysiz[0]/2, xysiz[1]-30, 'After correlation', ALIGNMENT=0.5, $
        COLOR=!KON.Colors.ColorVals[1]

img = 0

END  ;  DrawRefSrchImages

;***************************************************************************
PRO RefCmpDialog, pass, REF, CMP, cam_num, matcher_pix, half_patch_pix, $
                  edge_ring_pix, core_pix, offsets, xypt, Return
;***************************************************************************
; Show a dhow dialog box listing critical parameters if desired. This is
; activated with a hard-wired parameter and should be made a configurable
; parameter. To use, 1) turn on both SHOW_REF_CMP and SHOW_REF_CMP before
; building, 2) place a breakpoint on the last line before the ENDIF before
; running, 3) run, and when breakpoint is reached, make an image of the
; message box before killing it.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (pass EQ 1) THEN BEGIN
   ref_size_275  = REF.size_275_p1
   ref_size_1100 = REF.size_1100_p1
   cmp_size_275  = CMP.size_275_p1
   cmp_size_1100 = CMP.size_1100_p1
ENDIF ELSE BEGIN
   ref_size_275  = REF.size_275_p2
   ref_size_1100 = REF.size_1100_p2
   cmp_size_275  = CMP.size_275_p2
   cmp_size_1100 = CMP.size_1100_p2
ENDELSE

passnum = STRTRIM(STRING(pass),2)

titl = 'Camera ' + !KON.Instr.camera_names[cam_num+1] + $
       ' reference and comparison window parameters   '

hdr1 = '              Matcher Pass ' + passnum + $
       ' with Matcher Size = ' + STRTRIM(STRING(matcher_pix),2) + $
       ' x 275 m'
hdr2 = '                                             275 m ' + $
       '           1100 m'
hdr3 = '                                            -------' + $
       '          --------          '
str1 = 'Target pixel:'
str2 = '   Target pixel across/along         -'        + $
           STRING(FORMAT='(I7)',   REF.target_275[0])  + $
           STRING(FORMAT='(I8)',   REF.target_275[1])  + $
           STRING(FORMAT='(F13.2)',REF.target_1100[0]) + $
           STRING(FORMAT='(F8.2)', REF.target_1100[1])
str3 = 'Reference window:'
str4 = '   Window size across/along          -'    + $
           STRING(FORMAT='(I7)', ref_size_275[0])  + $
           STRING(FORMAT='(I8)', ref_size_275[1])  + $
           STRING(FORMAT='(I10)',ref_size_1100[0]) + $
           STRING(FORMAT='(I8)', ref_size_1100[1])
str5 = '   Window begin pixel index          -'   + $
           STRING(FORMAT='(I7)', REF.beg_275[0])  + $
           STRING(FORMAT='(I8)', REF.beg_275[1])  + $
           STRING(FORMAT='(I10)',REF.beg_1100[0]) + $
           STRING(FORMAT='(I8)', REF.beg_1100[1])
str6 = '   Window end pixel index            -'   + $
           STRING(FORMAT='(I7)', REF.end_275[0])  + $
           STRING(FORMAT='(I8)', REF.end_275[1])  + $
           STRING(FORMAT='(I10)',REF.end_1100[0]) + $
           STRING(FORMAT='(I8)', REF.end_1100[1])
str7 = '   Fractional pixel adjustment       -'         + $
           STRING(FORMAT='(I7)',   REF.beg_275_adj[0])  + $
           STRING(FORMAT='(I8)',   REF.beg_275_adj[1])  + $
           STRING(FORMAT='(F13.2)',REF.beg_1100_adj[0]) + $
           STRING(FORMAT='(F8.2)', REF.beg_1100_adj[1])
str8 = 'Comparison window:'
str9 = '   Components: half-ref/edge/core(2) -'   + $
           STRING(FORMAT='(I21)', half_patch_pix) + $
           STRING(FORMAT='(I4)', edge_ring_pix)   + $
           STRING(FORMAT='(I7)', core_pix[0])     + $
           STRING(FORMAT='(I4)', core_pix[1])
str10= '   Target pixel across/along         -'        + $
           STRING(FORMAT='(F10.2)',CMP.target_275[0])  + $
           STRING(FORMAT='(F8.2)', CMP.target_275[1])  + $
           STRING(FORMAT='(F10.2)',CMP.target_1100[0]) + $
           STRING(FORMAT='(F8.2)', CMP.target_1100[1])
str11= '   Window size across/along          -'    + $
           STRING(FORMAT='(I7)', cmp_size_275[0])  + $
           STRING(FORMAT='(I8)', cmp_size_275[1])  + $
           STRING(FORMAT='(I10)',cmp_size_1100[0]) + $
           STRING(FORMAT='(I8)', cmp_size_1100[1])
str12 ='   Window begin pixel index          -'   + $
           STRING(FORMAT='(I7)', CMP.beg_275[0])  + $
           STRING(FORMAT='(I8)', CMP.beg_275[1])  + $
           STRING(FORMAT='(I10)',CMP.beg_1100[0]) + $
           STRING(FORMAT='(I8)', CMP.beg_1100[1])
str13 ='   Window end pixel index            -'   + $
           STRING(FORMAT='(I7)', CMP.end_275[0])  + $
           STRING(FORMAT='(I8)', CMP.end_275[1])  + $
           STRING(FORMAT='(I10)',CMP.end_1100[0]) + $
           STRING(FORMAT='(I8)', CMP.end_1100[1])
str14 ='   Offset to target from cmp origin  -'        + $
           STRING(FORMAT='(I7)', CMP.totarget_275[0])  + $
           STRING(FORMAT='(I8)', CMP.totarget_275[1])  + $
           STRING(FORMAT='(I10)',CMP.totarget_1100[0]) + $
           STRING(FORMAT='(I8)', CMP.totarget_1100[1])

str15= 'Image display windows:'
str16= '   Image "+" offsets (pre-corr)      - '   + $
           STRING(FORMAT='(F27.2)',FLOAT(xypt[0])) + $
           STRING(FORMAT='(F8.2)', FLOAT(xypt[1]))
str17= '   Image "X" offsets (post-corr)     - '              + $
           STRING(FORMAT='(F27.2)',FLOAT(xypt[0])+offsets[0]) + $
           STRING(FORMAT='(F8.2)', FLOAT(xypt[1])+offsets[1])


str18='         Press <Yes> to continue, <No> to cancel these displays.'


IF (pass EQ 1) THEN BEGIN
   mssg = [' ', hdr1, '', hdr2, hdr3, str1, str2, '', str3, str4, str5, $
          str6, str7, '', str8, str9, str11, str12, str13, str14, $
          '', str15, str16, str17, '', '', str18, '']
ENDIF ELSE BEGIN
   mssg = [' ', hdr1, '', hdr2, hdr3, str1, str2, '', str3, str4, str5, $
          str6, str7, '', str8, str9, str10, str11, str12, str13, str14, $
          '', str15, str16, str17, '', '', str18, '']
ENDELSE

Return = DIALOG_MESSAGE(mssg, TITLE=titl, /QUESTION, /CENTER)

size_275  = 0
size_1100 = 0

END  ;  RefCmpDialog
