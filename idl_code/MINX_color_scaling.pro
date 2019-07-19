;===========================================================================
;                                                                          =
;                                     MINX                                 =
;                                                                          =
;===========================================================================
;                                                                          =
;                         Jet Propulsion Laboratory                        =
;                                   MISR                                   =
;                                                                          =
;         Copyright 2007-2019, California Institute of Technology.         =
;                           ALL RIGHTS RESERVED.                           =
;                U.S. Government Sponsorship acknowledged.                 =
;                                                                          =
;===========================================================================

;***************************************************************************
PRO SaveMissingPixelsInImage, NumCam, NumBand, Retval
;***************************************************************************
; Fill 2 global arrays of pointers dimensioned by band and camera. The first
; references an array of pixel indexes where there are terrain holes inside
; the swath. The second references an array of BRF values with which the
; holes are to be replaced when color scaling is performed.
;---------------------------------------------------------------------------

Retval = 0

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Loop over the cameras and bands.
;---------------------------------------------------------------------------

FOR icam=0,NumCam-1 DO BEGIN
   FOR iband=0,NumBand-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Get this band's image. Find indices inside the swath where data are
      ; missing (BRF = 0).
      ;---------------------------------------------------------------------

      pImage = !VAR.RawImages[iband,icam]
      ndxs_eq0 = WHERE(*pImage EQ 0.0, numndxs_eq0)
      IF (numndxs_eq0 EQ 0) THEN BEGIN
         !VAR.NullPixelList[iband,icam] = PTR_NEW([0])
         !VAR.NullValueList[iband,icam] = PTR_NEW([0])
         ndxs_eq0 = 0
         CONTINUE
      ENDIF

      band_smooth = *pImage
      image_size = SIZE(band_smooth)

      ;---------------------------------------------------------------------
      ; Copy missing indices to the global pointer array if there are any.
      ;---------------------------------------------------------------------

      IF (numndxs_eq0 GT 0) THEN BEGIN
         !VAR.NullPixelList[iband,icam] = PTR_NEW(ndxs_eq0)
         
         ndxs_eq0_prev = ndxs_eq0
         numndxs_eq0_prev = numndxs_eq0
  
         WHILE(numndxs_eq0 GT 0) DO BEGIN
            band_smooth[ndxs_eq0] = band_smooth[ndxs_eq0-1] > $
                                    band_smooth[ndxs_eq0+1] > $
                                    band_smooth[ndxs_eq0-image_size[1]] > $
                                    band_smooth[ndxs_eq0+image_size[1]]
            ndxs_eq0 = WHERE(band_smooth EQ 0.0, numndxs_eq0)
            IF (numndxs_eq0 EQ numndxs_eq0_prev) THEN BREAK
            numndxs_eq0_prev = numndxs_eq0
         ENDWHILE
         
         !VAR.NullValueList[iband,icam] = PTR_NEW(band_smooth[ndxs_eq0_prev])
      ENDIF ELSE BEGIN
         !VAR.NullPixelList[iband,icam] = PTR_NEW([0])
         !VAR.NullValueList[iband,icam] = PTR_NEW([0])
      ENDELSE
      
      ndxs_eq0 = 0
      ndxs_eq0_prev = 0
      band_smooth = 0
      image_size = 0
      
   ENDFOR
ENDFOR

END  ;  SaveMissingPixelsInImage

;***************************************************************************
PRO FillMissingPixelsInBRFs, Patch, Retval
;***************************************************************************
; Fill the terrain holes in an image patch to be used in image matching.
;---------------------------------------------------------------------------

Retval = 0

;---------------------------------------------------------------------------
; Find indices inside the patch where data are missing (BRF = 0).
;---------------------------------------------------------------------------

ndxs_eq0 = WHERE(Patch EQ 0.0, numndxs_eq0)
IF (numndxs_eq0 EQ 0) THEN RETURN

ndxs_eq0_2d = ARRAY_INDICES(Patch, ndxs_eq0)

patch_size = SIZE(Patch)
ndxs_OK = WHERE(ndxs_eq0_2d[1,*] NE 0 AND $
                ndxs_eq0_2d[1,*] NE patch_size[2]-1, numndxs_OK)
IF (numndxs_OK GT 0) THEN BEGIN
   numndxs_eq0 = numndxs_OK
   ndxs_eq0 = ndxs_eq0[ndxs_OK]
   ndxs_eq0_2d = ARRAY_INDICES(Patch, ndxs_eq0)
ENDIF

ndxs_OK = 0

;---------------------------------------------------------------------------
; Fill the holes.
;---------------------------------------------------------------------------

IF (numndxs_eq0 GT 0) THEN BEGIN

   ndxs_eq0_prev = ndxs_eq0
   numndxs_eq0_prev = numndxs_eq0
   
   WHILE(numndxs_eq0 GT 0) DO BEGIN
      Patch[ndxs_eq0] = Patch[ndxs_eq0-1] > $
                        Patch[ndxs_eq0+1] > $
                        Patch[ndxs_eq0-patch_size[1]] > $
                        Patch[ndxs_eq0+patch_size[1]]
      ndxs_eq0 = WHERE(Patch EQ 0.0, numndxs_eq0)
      IF (numndxs_eq0 EQ numndxs_eq0_prev) THEN BREAK
      numndxs_eq0_prev = numndxs_eq0
   ENDWHILE
   
ENDIF
   
END  ;  FillMissingPixelsInBRFs

;***************************************************************************
PRO RegressBandAgainstRed, x1, x2, y1, y2, Bands, iCam, RedXsize, RedYsize, $
                           STARTUP, RETRIEVAL, TempAry, Retval
;***************************************************************************
; For L1 non-nadir camera images, the blue, green and NIR bands are at 1100
; m resolution, while the red band is at 275 m. Regress the 1100 m bands
; against the red to create pseudo-275 m resolution images. The ratio of the
; 16 pixels in each red 1100 m square is maintained in the new image, while
; total radiance is conserved (no longer - see below).
; ------>   NOTE - the red band is in array position 0 : [R,G,B,N].  <------
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

Retval = -1

NbandWant = N_ELEMENTS(Bands)

;---------------------------------------------------------------------------
; Pre-process the red band if any non-red band is low res. Fill in missing
; pixels with smoothed values derived during image loading. This is needed
; so the 1100 m image isn't messed up when regression is applied below. We
; need to find where red pixels are LE 0 as well as EQ 0, because pixels
; internal to image that are invalid due to obscuration by terrain have
; values of 0, while values on the no-data edges of the image have values
; of -1. Don't fill values on the diagonal no-data edges.
;---------------------------------------------------------------------------

save_x1 = x1

red_275 = GetRawImage(x1, x2, y1, y2, 0, iCam, !KON.Instr.HI_RES_PIX_SIZE, $
                      !KON.Misc.INTERP_TYPE_SAMP)
ndxs_red_le0 = WHERE(red_275 LE 0.0, numndxs_red_le0)

IF (RETRIEVAL) THEN BEGIN
   FillMissingPixelsInBRFs, red_275, Retval
ENDIF ELSE BEGIN
   IF (PTR_VALID(!VAR.NullPixelList[0,iCam])) THEN $
      red_275[*(!VAR.NullPixelList[0,iCam])] = *(!VAR.NullValueList[0,iCam])
ENDELSE

;---------------------------------------------------------------------------
; Create a temporary nband array at high resolution to return to caller.
;---------------------------------------------------------------------------

TempAry = FLTARR(RedXsize, RedYsize, NbandWant)

;---------------------------------------------------------------------------
; Loop over the bands.
;---------------------------------------------------------------------------

FOR iBand=0,NbandWant-1 DO BEGIN

   xBand = Bands[iBand]

   ;------------------------------------------------------------------------
   ; For hi-res data, only need to get the data and copy it into output - no
   ; resampling needed. The nadir camera is taken care of in caller, so no
   ; need to handle its blue, green and NIR hi-res channels here.
   ;------------------------------------------------------------------------

   IF ((SIZE(*!VAR.RawImages[xBand,iCam]))[1] GE $
              !KON.Instr.HI_RES_PIX_CROSS) THEN BEGIN
      IF (xBand EQ !KON.Instr.RED) THEN BEGIN
         band_ary = red_275
      ENDIF ELSE BEGIN
         band_ary = GetRawImage(x1, x2, y1, y2, 0, iCam, $
                                !KON.Instr.HI_RES_PIX_SIZE, $
                                !KON.Misc.INTERP_TYPE_SAMP)
      ENDELSE

   ;------------------------------------------------------------------------
   ; Get BRF data for next low resolution band. x1, x2, y1, y2 must be on
   ; multiple of 4 boundaries.
   ;------------------------------------------------------------------------

   ENDIF ELSE BEGIN     ; 1100 meter bands here

      IF (save_x1 NE -1) THEN BEGIN
         x1 /= 4
         x2 = (x2 + 1) / 4 - 1
         y1 /= 4
         y2 = (y2 + 1) / 4 - 1
      ENDIF

      nonred_1100 = GetRawImage(x1, x2, y1, y2, xBand, iCam, $
                                !KON.Instr.LO_RES_PIX_SIZE, $
                                !KON.Misc.INTERP_TYPE_SAMP)

      ;---------------------------------------------------------------------
      ; Fill in the missing pixels (but not those on the diagonal edges) with
      ; smoothed  pixels obtained during image loading.
      ;---------------------------------------------------------------------

      IF (RETRIEVAL) THEN BEGIN
         FillMissingPixelsInBRFs, nonred_1100, Retval      
      ENDIF ELSE BEGIN
         IF (PTR_VALID(!VAR.NullPixelList[xBand,iCam])) THEN $
            nonred_1100[*(!VAR.NullPixelList[xBand,iCam])] = $
                        *(!VAR.NullValueList[xBand,iCam])
      ENDELSE
                     
      ;---------------------------------------------------------------------
      ; Reduce the red band's resolution to the same low resolution of the
      ; nonred band using nearest neighbor sampling.
      ;---------------------------------------------------------------------
                  
      red_1100 = REBIN(red_275, RedXsize/4, RedYsize/4)
      ndxs = WHERE(red_1100 GT 0.0 AND nonred_1100 GT 0.0, numndxs)
      
      ;---------------------------------------------------------------------
      ; Regress non-red low-res bands against the high-res red band to
      ; produce all high-resolution bands:
      ; 1) Construct an array containing the ratios of BRFs for the lo-res
      ;    band to BRFs for the down-sampled (1100 m) red band in same camera.
      ; 2) Resample the ratio array to hi-res using nearest neighbor sampling
      ;    (INTERP_TYPE_SAMP ?) to ensure all 16 ratios are the same within
      ;    every 4x4 square.
      ; 3) Multiply the hi-res ratio array by the hi-res red array to get a
      ;    hi-res non-red array.
      ;---------------------------------------------------------------------
      
      IF (numndxs GT 0) THEN BEGIN
         ratio_1100 = nonred_1100 * 0.0
         ratio_1100[ndxs] = nonred_1100[ndxs] / red_1100[ndxs]
         
         interp_type = RETRIEVAL ? !KON.Misc.INTERP_TYPE_CUBIC : $
                                   !KON.Misc.INTERP_TYPE_BILIN
         
         band_ary = RebinExpand(ratio_1100, RedXsize, RedYsize, interp_type) * $
                                red_275

         ndxs = WHERE(band_ary LT 0.0 OR band_ary GT 4.0, numndxs)
         IF (numndxs GT 0) THEN band_ary[ndxs] = 0.0 
      ENDIF

      ndxs = 0
      ratio_1100 = 0
      red_1100 = 0
      nonred_1100 = 0

   ENDELSE

   ;------------------------------------------------------------------------
   ; Restore bad pixels in all bands where they were found in the red band.
   ;------------------------------------------------------------------------

   IF (!VAR.CurrFiles.TerrHoles_Loaded) THEN $
      IF (numndxs_red_le0 GT 0) THEN band_ary[ndxs_red_le0] = 0.0

   ;------------------------------------------------------------------------
   ; Save processed band array.
   ;------------------------------------------------------------------------

   good_ndx = WHERE(band_ary GT 0.0, numgood)

   IF (numgood LT 1 AND ~RETRIEVAL) THEN BEGIN
      bnd_names = ['Red', 'Green', 'Blue', 'NIR']
      mssg = ['No valid data were found in camera/band : ' + $
              !KON.Instr.camera_names[iCam+1] + '/' + bnd_names[xBand], $
              'Try different blocks in this orbit or a different orbit.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   TempAry[*,*,[xBand]] = band_ary
   good_ndx = 0 
   band_ary = 0

ENDFOR

Retval = 0

END  ;  RegressBandAgainstRed

;***************************************************************************
PRO RescaleColorsForDiffBands, State, UseBands
;***************************************************************************
; Rescale colors in animation window for all cameras by operating on raw BRF
; data. This requires regressing the 1.1 km channels to .275 km also. For
; non-true-color scaling, each channel is scaled independently of the others.
; For true-color scaling, all channels are scaled to the same range.
;---------------------------------------------------------------------------
   
   COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
   ;------------------------------------------------------------------------
   ; Initialize parameters.
   ;------------------------------------------------------------------------
   
   save_curframe = State.curframe
   SWIN = !D.WINDOW
   
   ;------------------------------------------------------------------------
   ; Loop over the cameras.
   ;------------------------------------------------------------------------
   
   numcam = !KON.Instr.NCAM * !VAR.NumOrbits
   
   FOR icam=0,numcam-1 DO BEGIN
   
      PrepImageArray, 0, UseBands, numcam, icam, image, retval
      
      ;---------------------------------------------------------------------
      ; Write image back to graphics memory.
      ;---------------------------------------------------------------------
      
      SafeWSET, (*State.pwinHdl)[icam+1], didit
      
      TV, image, /ORDER, TRUE=((N_ELEMENTS(UseBands) GE 3) ? 3 : 0)
      RedrawWindow, State, icam + 1
      image = 0
      
   ENDFOR
   
   ;------------------------------------------------------------------------
   ; Reset the current window to be the one current before scaling.
   ;------------------------------------------------------------------------
   
   State.curframe = save_curframe
   SafeWSET, SWIN, didit
   
   RedrawWindow, State, State.curframe
   
END  ;  RescaleColorsForDiffBands

;***************************************************************************
PRO PrepImageArray, STARTUP, UseBands, NumCam, iCam, Image, Retval
;***************************************************************************
; Scale the colors in each channel to the requested ranges and produce an
; array of camera images in RGB or grayscale (one camera per invocation of
; this procedure). For RGB images when first loaded, each camera's bands
; are scaled independently. Subsequent rescalings can be chosen to result in
; the bands for all cameras scaled to the MEAN of all channels (NIR is
; excluded from these scalings unless it is the only band shown).
;---------------------------------------------------------------------------

COMMON chan_minmax, ChanMinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the number of bands per camera desired (either 1 for gray-scale in a
; single band or 3 for RGB), the size of the single band for this camera and
; the sizes of the channels to be processed. Test if each channel is low res.
; ------>   NOTE - the red band is in array position 0 : [R,G,B,N].  <------
;---------------------------------------------------------------------------

nband_want = N_ELEMENTS(UseBands)
any_lowres = 0

sizes = SIZE(*!VAR.RawImages[0,iCam])
red_xsize = sizes[1]
red_ysize = sizes[2]

FOR iBand=0,nband_want-1 DO BEGIN
   sizes = SIZE(*!VAR.RawImages[UseBands[iBand], iCam])
   IF ((sizes[1] * 4) EQ red_xsize AND (sizes[2] * 4) EQ red_ysize) THEN BEGIN
      any_lowres = 1
      BREAK
   ENDIF
ENDFOR

sizes = 0

;---------------------------------------------------------------------------
; Call the routine to regress the 1100 m bands against the red band for this
; camera. If ALL bands are at high resolution, this isn't necessary.
;---------------------------------------------------------------------------

IF (any_lowres) THEN BEGIN
   RegressBandAgainstRed, -1, -1, -1, -1, UseBands, iCam, red_xsize, red_ysize, $
                          STARTUP, 0, temp_ary, Retval
   IF (Retval LT 0) THEN BEGIN
      temp_ary = 0
      RETURN
   ENDIF
ENDIF ELSE BEGIN 
   temp_ary = FLTARR(red_xsize, red_ysize, nband_want)

   FOR iBand=0,nband_want-1 DO BEGIN
      band_275 = GetRawImage(-1, -1, -1, -1, UseBands[iBand], iCam, $
                             !KON.Instr.HI_RES_PIX_SIZE, $
                             !KON.Misc.INTERP_TYPE_SAMP)
      ndxs_band_lt0 = WHERE(band_275 LT 0.0, numndxs_band_lt0)
      IF (numndxs_band_lt0 GT 0) THEN band_275[ndxs_band_lt0] = 0.0
      
      temp_ary[*,*,iBand] = band_275
   ENDFOR

   ndxs_band_lt0 = 0
   band_275 = 0
ENDELSE

;---------------------------------------------------------------------------
; On first entry, calculate the effective minimum and maximum RGB values for
; each channel using histograms. Set the minima to 0. Cut off the upper tail
; of each BRF histogram according to a fixed minimum fraction of BRFs per
; bin. The last non-zero bin remaining is the maximum effective value.
; On subsequent entries, just set the param that controls whether the color
; scale factor is different for each channel (on startup we have only one
; camera at a time) or the same for all cameras.
;---------------------------------------------------------------------------

IF (STARTUP EQ 1) THEN BEGIN
   IF (iCam EQ 0) THEN ChanMinMax = FLTARR(4,!KON.Instr.NBAND, NumCam)
   GetBaselineColorHistogram, temp_ary, iCam, (nband_want EQ 1) ? [0] : [0,1,2,3]
ENDIF

;---------------------------------------------------------------------------
; Get initial min/max scaling values for each band of this camera. Adjust
; the min and max values to improve bright and contrast under differing
; conditions. Create the image for this camera.
;---------------------------------------------------------------------------

nband = N_ELEMENTS(UseBands) - 1
RescaleColorsFromSliders, REFORM(temp_ary[*,*,0:nband]), STARTUP, UseBands, $
                          iCam, Image

;---------------------------------------------------------------------------
; Return the new RGB image for this camera.
;---------------------------------------------------------------------------

Retval = 0

END  ; PrepImageArray

;***************************************************************************
PRO GetBaselineColorHistogram, BrfArray, iCam, UseBands
;***************************************************************************
; Calculate the effective minimum and maximum RGB values for each channel
; using histograms. Set the minima to 0. Cut off the tails of each BRF
; histogram according to fixed minimum and maximum fractions of BRFs.
;---------------------------------------------------------------------------

COMMON chan_minmax, ChanMinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

DISPLAY_HIST = 0     ; set to 0 for production

;---------------------------------------------------------------------------
; For each band, construct a histogram of the BRF values and save min, max
; and modal values of the BRF array truncated to remove probable outliers.
; ------>   NOTE - the red band is in array position 0 : [R,G,B,N].  <------
;---------------------------------------------------------------------------

nband_want = N_ELEMENTS(UseBands)

FOR iBand=0,nband_want-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Collect all the acceptable BRF values in an array..
   ;------------------------------------------------------------------------
   
   vals = REFORM(BrfArray[*,*,UseBands[iBand]])
   min_val = 0.0
   max_val = MAX(vals) < 1.2
   ndxs = WHERE(vals GE min_val AND vals LE max_val, numndx)
   IF (numndx EQ 0) THEN RETURN
   vals = vals[ndxs]
   ndxs = 0
      
   ;------------------------------------------------------------------------
   ; Construct a histogram of BRFs.
   ;------------------------------------------------------------------------
   
   bin_size = max_val / (!VAR.ClrScaleVals.NumHistBins - 1)
   hist1 = HISTOGRAM(vals, NBINS=!VAR.ClrScaleVals.NumHistBins)
   
   ;------------------------------------------------------------------------
   ; Cut off the low-end tail.
   ;------------------------------------------------------------------------
   
   ibin_min = 0
   num_cut = 0
   num_cut_min = numndx * !VAR.ClrScaleVals.FracCutTailMin
   WHILE (ibin_min LT !VAR.ClrScaleVals.NumHistBins-1 AND $
          num_cut LT num_cut_min) DO BEGIN
      num_cut += hist1[ibin_min]
      ibin_min += 1
   ENDWHILE
   min_val = bin_size * ibin_min

   ;------------------------------------------------------------------------
   ; Cut off the high-end tail.
   ;------------------------------------------------------------------------
   
   ibin_max = !VAR.ClrScaleVals.NumHistBins-1
   num_cut = 0
   num_cut_max = numndx * !VAR.ClrScaleVals.FracCutTailMax
   WHILE (ibin_max GT ibin_min AND num_cut LT num_cut_max) DO BEGIN
      num_cut += hist1[ibin_max]
      ibin_max -= 1
   ENDWHILE
   max_val = bin_size * ibin_max

   mean_value = MEAN(vals)
   brf_diff = (ABS(max_val - mean_value) / 4.0) < 0.3
      
   vals = 0
   
   ;------------------------------------------------------------------------
   ; Save the min, max and mode for this channel.
   ;------------------------------------------------------------------------
   
   ChanMinMax[0,UseBands[iBand],iCam] = min_val
   ChanMinMax[1,UseBands[iBand],iCam] = max_val
   ChanMinMax[2,UseBands[iBand],iCam] = mean_value ; not used
   ChanMinMax[3,UseBands[iBand],iCam] = brf_diff

   ;---------------------------------------------------------------------------
   ; If debugging, choose to display histograms.
   ;---------------------------------------------------------------------------

   IF (DISPLAY_HIST) THEN BEGIN
      WINDOW, 11, XSIZE=800, YSIZE=400, XPOS=1400, YPOS=250, TITLE='Cam ' + $
              STRTRIM(STRING(iCam),2) + ', Band ' + STRTRIM(STRING(iBand),2) + $
              '; Min =' + STRING(FORMAT='(F5.2)', min_val) + $
              ', Max =' + STRING(FORMAT='(F5.2)', max_val)
      PLOT, INDGEN(N_ELEMENTS(hist1)), hist1, XSTYLE=1, XRANGE=[0,!VAR.ClrScaleVals.NumHistBins-1]
      OPLOT, [min_val/bin_size,min_val/bin_size], [0,MAX(hist1)], LINESTYLE=0, $
             COLOR=!KON.Colors.red, THICK=1
      OPLOT, [max_val/bin_size,max_val/bin_size], [0,MAX(hist1)], LINESTYLE=0, $
             COLOR=!KON.Colors.red, THICK=1
      OPLOT, [top_ndx,top_ndx], [0,MAX(hist1)], LINESTYLE=0, THICK=1, $
             COLOR=!KON.Colors.blue
      rval = DIALOG_MESSAGE('Press <Enter> to continue', /CENTER, /INFO)
      WDELETE, 11
   ENDIF

   hist1 = 0

ENDFOR

END  ;  GetBaselineColorHistogram

;***************************************************************************
PRO RescaleColorsFromSliders, BrfArray, Startup, UseBands, iCam, Image
;***************************************************************************
; Implement brightness and contrast scaling based on the positions of slider
; controls. Scaling differs for Equalized color (all 36 channels scaled to
; same min/max = 0) vs independent color per camera (all bands in each camera
; scaled to same min/max = 1) vs independent color per channel (each band in
; each camera scaled independently = 2 = default).
;---------------------------------------------------------------------------

COMMON top_level_base, wTopWorkBase
COMMON chan_minmax, ChanMinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the first and last bands to use in computing the min and max BRF to
; use in scaling the colors for this camera. 
;---------------------------------------------------------------------------

nband_want = N_ELEMENTS(UseBands) < 3  ; if 4 bands input, don't scale NIR
fsize = SIZE(BrfArray)

beg_bnd = UseBands[0]
end_bnd = UseBands[nband_want-1]

Image = REFORM(BYTARR(fsize[1], fsize[2], nband_want))

;---------------------------------------------------------------------------
; Loop over bands if to be scaled independently.
;---------------------------------------------------------------------------

ScalingMethod = !VAR.ClrScaleVals.ScalingMethod
IF (ScalingMethod EQ 2 AND $
    MAX(ChanMinMax[1, beg_bnd:end_bnd, iCam]) GT $
    MIN(ChanMinMax[1, beg_bnd:end_bnd, iCam]) * 1.5) THEN ScalingMethod = 1
   
FOR iband=0,nband_want-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Find reasonable default values for the min and max BRFs to assign to
   ; the color range of 0 -> 255. Also get the slice of the input BRF array
   ; to be processed.    per-channel: ChanMinMax[1, beg_bnd+iband, iCam]
   ;------------------------------------------------------------------------

   SWITCH ScalingMethod OF
      0: BEGIN
         beg_val  = MIN( ChanMinMax[0, beg_bnd:end_bnd, *])
         end_val  = MAX( ChanMinMax[1, beg_bnd:end_bnd, *])
         brf_diff = MEAN(ChanMinMax[3, beg_bnd:end_bnd, *])
         BREAK
      END
      1: BEGIN
         beg_val  = MIN( ChanMinMax[0, beg_bnd:end_bnd, iCam])
         end_val  = MAX( ChanMinMax[1, beg_bnd:end_bnd, iCam])
         brf_diff = MEAN(ChanMinMax[3, beg_bnd:end_bnd, iCam])
         BREAK
      END
      2: BEGIN
         beg_val  = ChanMinMax[0, UseBands[iband], iCam]
         end_val  = ChanMinMax[1, UseBands[iband], iCam]
         brf_diff = ChanMinMax[3, UseBands[iband], iCam]
         BREAK
      END
   ENDSWITCH

   IF (Startup AND iband EQ 0) THEN contrast_new = $
      !VAR.ClrScaleVals.Contrast - brf_diff
   IF (~Startup) THEN contrast_new = !VAR.ClrScaleVals.Contrast

   brf_slice = REFORM(BrfArray[*,*,iband])
      
   ;------------------------------------------------------------------------
   ; Get the pixel indices in the image corresponding to good and bad values.
   ;------------------------------------------------------------------------

   ndxbad = WHERE(brf_slice LE 0.0, numbad, COMPLEMENT=ndxgood, $
                  NCOMPLEMENT=numgood)

   ;------------------------------------------------------------------------
   ; Perform contrast adjustment and brightness scaling here. Then create
   ; the image.
   ;------------------------------------------------------------------------

   IF (numgood GT 0) THEN BEGIN
   
      ;---------------------------------------------------------------------
      ; Scale the BRFs for contrast. Construct the contrast exponent from
      ; the contrast slider value. A contrast slider value of 0.0 maps to
      ; exp = 0.25, and a contrast slider value of 1.0 maps to exp = 2.25.
      ;---------------------------------------------------------------------

      IF (ABS(contrast_new - 0.5) GT 0.005) THEN BEGIN
         contrast = (contrast_new + 0.5) ^ 2
         brf_slice[ndxgood] ^= contrast
         beg_val ^= contrast
         end_val ^= contrast
      ENDIF
   
      ;------------------------------------------------------------------------
      ; Scale BRFs for brightness. Construct the brightness coefficient from
      ; the brightness slider value. A brightness slider value of 0.0 maps to
      ; bright = -0.5 * max_bright, and a brightness slider value of 1.0 maps
      ; to bright = 0.5 * max_bright.
      ;------------------------------------------------------------------------
   
      IF (ABS(!VAR.ClrScaleVals.Brightness - 0.5) GT 0.005) THEN BEGIN
         bright = (!VAR.ClrScaleVals.Brightness - 0.5) * $
                  ChanMinMax[1, UseBands[iband], iCam]
         brf_slice[ndxgood] += bright
         beg_val += bright / 2.0
         end_val += bright / 2.0
      ENDIF
   
      ;------------------------------------------------------------------------
      ; Fill the channel(s) of the image array with Byte-scaled values.
      ;------------------------------------------------------------------------

      brf_slice = BYTSCL(TEMPORARY(brf_slice), MIN=beg_val, MAX=end_val)
      IF (numbad GT 0) THEN brf_slice[ndxbad] = 0
      Image[*,*,iband] = brf_slice

   ENDIF
ENDFOR

;---------------------------------------------------------------------------
; Change the contrast slider to the value of the An contrast.
;---------------------------------------------------------------------------

IF (iCam EQ !KON.Instr.AN AND ABS(contrast_new - 0.5) GT 0.005) THEN BEGIN
      !VAR.ClrScaleVals.Contrast = contrast_new
      slider_cntl1 = FIX(!VAR.ClrScaleVals.Contrast * 100)
      slider_val1  = STRING(!VAR.ClrScaleVals.Contrast, FORMAT='(F4.2)')
      WIDGET_CONTROL, wTopWorkBase, GET_UVALUE=state
      WIDGET_CONTROL, state.wContrastCntlSlider, SET_VALUE = slider_cntl1
      WIDGET_CONTROL, state.wContrastValue, SET_VALUE = slider_val1
      state = 0
ENDIF

brf_slice = 0

;---------------------------------------------------------------------------
; Return the new RGB image for this camera.
;---------------------------------------------------------------------------

END  ;  RescaleColorsFromSliders









