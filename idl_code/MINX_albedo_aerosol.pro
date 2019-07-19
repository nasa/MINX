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

;************************************************************************
PRO SolidAngleAlbedo, CamNum, NCAM, CosCamZenAng, Brfs, Albedo, Retval
;************************************************************************
; Compute the narrow-band albedo at a point for 9 cameras.  The
; algorithm is a simplified copy of the PGE8C algorithm.
;-------------------------------------------------------------------

Retval = -1

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;-------------------------------------------------------------------
; Set up the fixed values of the solid angle weighting factors from
; the PGE8C AZM dataset.
;-------------------------------------------------------------------

SOLID_ANG_WTS = $
   [[-9.9900E+02,  6.9300E-03, -9.9900E+02, -9.9900E+02, -9.9900E+02, $
     -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02], $
    [ 1.0380E-02,  6.8625E-02,  1.2500E-02, -9.9900E+02, -9.9900E+02, $
     -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02], $
    [-9.9900E+02,  1.7500E-02,  1.0448E-01,  1.7330E-02, -9.9900E+02, $
     -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02], $
    [-9.9900E+02, -9.9900E+02,  2.2230E-02,  9.5300E-02,  9.5400E-03, $
     -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02], $
    [-9.9900E+02, -9.9900E+02, -9.9900E+02,  1.0440E-02, -9.9900E+02, $
      1.0440E-02, -9.9900E+02, -9.9900E+02, -9.9900E+02], $
    [-9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02,  9.5400E-03, $
      9.5300E-02,  2.2230E-02, -9.9900E+02, -9.9900E+02], $
    [-9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, $
      1.7330E-02,  1.0448E-01,  1.7500E-02, -9.9900E+02], $
    [-9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, $
     -9.9900E+02,  1.2500E-02,  6.8625E-02,  1.0380E-02], $
    [-9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, -9.9900E+02, $
     -9.9900E+02, -9.9900E+02,  6.9300E-03, -9.9900E+02]]

SAW_BADVALUE= -9.99E+02

;-------------------------------------------------------------------
; Establish index values for the current and adjacent camera(s).
;-------------------------------------------------------------------

IF (CamNum EQ 0) THEN BEGIN
    cama = CamNum                 ; Current camera.
    camb = CamNum + 1             ; Next camera.
    num_terms = 2                 ; Number of terms over which to sum.
ENDIF ELSE BEGIN
   IF (CamNum EQ !KON.Instr.NCAM-1) THEN BEGIN
      cama = CamNum               ; Current camera.
      camb = CamNum - 1           ; Previous camera.
      num_terms = 2               ; Number of terms over which to sum.
   ENDIF ELSE BEGIN
      cama = CamNum - 1           ; Previous camera.
      camb = CamNum               ; Current camera.
      camc = CamNum + 1           ; Next camera.
      num_terms = 3               ; Number of terms over which to sum.
   ENDELSE
ENDELSE

;-------------------------------------------------------------------
; Save the BRF values for current camera and the adjacent camera(s) 
; in new variables so indices are compatible with other variables.
;-------------------------------------------------------------------

brf = FLTARR(3)

brf[0] = Brfs[cama]
brf[1] = Brfs[camb]
IF (CamNum GT 0 AND CamNum LT !KON.Instr.NCAM-1) THEN BEGIN
   brf[2] = Brfs[camc]
ENDIF

;----------------------------------------------------------------------
; Test if any BRF value is missing. If so, we canot compute the albedo
; contribution for this camera - it remains  BADVALUE. However, return
; w/ status code.
;----------------------------------------------------------------------

FOR i=0,num_terms-1 DO BEGIN
   IF (brf[i] LT 0.0) THEN BEGIN
      Retval = -2
      RETURN				    
   ENDIF 
ENDFOR

;-------------------------------------------------------------------------
; Determine the values of variables used in summing for local albedo
; contributions. Cameras 1, 5 and 9 are special cases. Note that the 
; SOLID_ANG_WTS values are fixed solid angle weights from the PGE8C AZM.
;-------------------------------------------------------------------------

mu = FLTARR(3)
wt_factor = FLTARR(3)

IF (CamNum EQ 0 OR CamNum EQ !KON.Instr.NCAM-1) THEN BEGIN

   ;----------------------------------------------------------------------
   ; Save the view zenith angle cosines in new variables so their
   ; indices are compatible with other variables.
   ;----------------------------------------------------------------------

   mu[0] = CosCamZenAng[cama]
   mu[1] = CosCamZenAng[camb]

   ;----------------------------------------------------------------------
   ; Calculate the value of the weighting factor for the current camera
   ; since it is not in the AZM database for cameras 1 and 9.
   ; Compute using ATB equations 35 and 37.
   ;----------------------------------------------------------------------

   wt_factor[0] = FLOAT((mu[0] / 8.D) * (3.D * mu[1] + mu[0]))
    
   ;----------------------------------------------------------------------
   ; Use SOLID_ANG_WTS[], the local albedo angular integration wts from the  
   ; AZM database, to retrieve the weighting factor for the adjacent camera.
   ;----------------------------------------------------------------------

   wt_factor[1] = SOLID_ANG_WTS[cama, camb]  ; Note order of indices.

ENDIF

IF (CamNum EQ 4) THEN BEGIN       ; ------- Handle An camera --------

   ;----------------------------------------------------------------------
   ; Save the view zenith angle cosines in new variables so their 
   ; indices are compatible with other variables.
   ;----------------------------------------------------------------------

   mu[0] = CosCamZenAng[cama]
   mu[1] = CosCamZenAng[camb]
   mu[2] = CosCamZenAng[camc]

   ;----------------------------------------------------------------------
   ; Calculate the value of the weighting factor for camera 5 since
   ; it is not in the AZM database. Compute using ATB equation 36. 
   ;----------------------------------------------------------------------

   wt_factor[1] = FLOAT((mu[1] / 8.D) * ((8.D0 / mu[1]) - 8.D + $
                        (8.0D - 2.D * mu[1] - 3.D * mu[0] - 3.D * mu[2])))   

   ;----------------------------------------------------------------------
   ; Use SOLID_ANG_WTS[], the local albedo angular integration wts from the  
   ; AZM database, to retrieve the weighting factor for the adjacent camera.
   ;----------------------------------------------------------------------

   wt_factor[0] = SOLID_ANG_WTS[camb,cama]
   wt_factor[2] = SOLID_ANG_WTS[camb,camc]

ENDIF

IF (CamNum NE 0 AND CamNum NE 4 AND CamNum NE !KON.Instr.NCAM-1) THEN BEGIN 
   ; ------- Handle cameras 2, 3, 4, 6, 7, 8. ---------

   ;----------------------------------------------------------------------
   ; Use SOLID_ANG_WTS[], the local albedo angular integration wts from the  
   ; AZM database, to retrieve the weighting factor for all the cameras.
   ;----------------------------------------------------------------------

   wt_factor[0] = SOLID_ANG_WTS[camb,cama]
   wt_factor[1] = SOLID_ANG_WTS[camb,camb]
   wt_factor[2] = SOLID_ANG_WTS[camb,camc]

ENDIF

;--------------------------------------------------------------------------
; Perform the summing of terms for calculation of local albedo contribution
; for this camera [CamNum]. Compute using ATB equation 33.
;--------------------------------------------------------------------------

Albedo[CamNum] = 0.           ; Do not do this earlier.

FOR i=0,num_terms-1 DO BEGIN
   Albedo[CamNum] = FLOAT(Albedo[CamNum] + wt_factor[i] * brf[i])
ENDFOR

;--------------------------------------------------------------------------
; Clean up and return.
;--------------------------------------------------------------------------

mu = 0
wt_factor = 0

Retval = 0

END  ;  SolidAngleAlbedo

;************************************************************************
PRO BroadBandAlbedo, CosSunZenAng, NB_Alb, BB_Alb, Retval
;************************************************************************
; Compute an approximate broadband albedo from the spectral albedos.
;-------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;-------------------------------------------------------------------
; Specify the value of the broadband conversion coefficients and
; the solar parameters.
;-------------------------------------------------------------------

alb_broadband_zeropoint = 1.640
alb_broadband_I0b = 1368.0
alb_broadband_coeff = [0.170, 0.210, -0.18, 0.530]

std_solar_wgted_height = [1871.294, 1851.302, 1524.957, 969.6405]
earth_sun_distance = 1.0  ;  in AU - this is close enough
es_dist_sq = earth_sun_distance * earth_sun_distance

;-------------------------------------------------------------------
; Return if sun zenith angle or any spectral albedo is not OK. Then
; initialize the broadband albedo.
;-------------------------------------------------------------------

BB_Alb = !KON.Misc.BADVALUE_REAL

ndxs = WHERE(NB_Alb LT 0.0, numzero)
ndxs = 0
IF ((numzero GT 0) OR (CosSunZenAng LT 0.0)) THEN RETURN

BB_Alb = alb_broadband_zeropoint

;-------------------------------------------------------------------
; Sum the spectral components and compute broadband value.
;-------------------------------------------------------------------

FOR iband=0,3 DO BEGIN

   BB_Alb += (alb_broadband_coeff[iband] * NB_Alb[iband] * $
              CosSunZenAng * std_solar_wgted_height[iband] / $
              es_dist_sq)
ENDFOR

BB_Alb /= (alb_broadband_I0b * CosSunZenAng / es_dist_sq)


Retval = 0

END  ;  BroadBandAlbedo

;************************************************************************
PRO ComputeAlbedos, State, NumPts, MisrCoords, OffsetsIn, SomCoords, $
                    CamZenithAng, SunZenithAng, NB_Albedos, $
                    BB_Albedos, Retval
;************************************************************************
; Compute the broadband albedos from the BRFs at each point on a plume
; profile. Use all 36 channels and solid-angle weighting. Albedo
; resolution is always 1100 m, regardless of sample spacing or image
; matcher size.
;-------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

;-------------------------------------------------------------------
; Create an array to contain the BRFs per channel for a point.
;-------------------------------------------------------------------

brfs    = FLTARR(9) + !KON.Misc.BADVALUE_REAL
alb_cam = FLTARR(9) + !KON.Misc.BADVALUE_REAL
along_max = State.sizey

;-------------------------------------------------------------------
; Band order is red, green blue, NIR.  Set up indexes to change this
; to MISR order.
;-------------------------------------------------------------------

band_index = [2,1,0,3]

;-------------------------------------------------------------------
; Change any offsets that are BAD_VALUE to 0.0.
;-------------------------------------------------------------------

Offsets = OffsetsIn
ndxs = WHERE(Offsets LT -999.0, numndxs)
IF (numndxs GT 0) THEN Offsets[ndxs] = 0.0

;-------------------------------------------------------------------
; Loop over the points on the profile.
;-------------------------------------------------------------------

FOR ipt=0,NumPts-1 DO BEGIN

   ;----------------------------------------------------------------
   ; If there are missing disparities but at least 4 good ones
   ; including An, use linear interpolation to fill them in.
   ;----------------------------------------------------------------

   new_offsets = Offsets * 0.0

   cross_off = REFORM(Offsets[0,*,ipt])
   along_off = REFORM(Offsets[1,*,ipt])
   ndxgood = WHERE(ABS(cross_off) LT 40 AND ABS(along_off) LT 40 AND $
                   cross_off NE 0 AND along_off NE 0, numgood)

   IF (numgood GE 4) THEN BEGIN
      new_offsets[0,*,ipt] = ROUND(INTERPOL(cross_off[ndxgood], ndxgood, $
                                           [0,1,2,3,4,5,6,7,8]))
      new_offsets[1,*,ipt] = ROUND(INTERPOL(along_off[ndxgood], ndxgood, $
                                           [0,1,2,3,4,5,6,7,8]))
   ENDIF ELSE BEGIN
      new_offsets[*,*,ipt] = !KON.Misc.BADVALUE_REAL
   ENDELSE

   ndxgood = 0

   ;----------------------------------------------------------------
   ; Get the pixel coordinate, then the data coordinate for this
   ; point.  We must 'invert' the along coordinate.  Also move the
   ; coordinate by the amount of the stereo matcher disparity to
   ; point the cameras at the same object. 
   ;----------------------------------------------------------------

   MisrCrdToWndwCrd, State.curframe, MisrCoords[*,ipt], wndw_crds, $
                     1, retval
   icross = wndw_crds[0,0]
   ialong = along_max - wndw_crds[1,0] - 1

   ;----------------------------------------------------------------
   ; Get the camera and sun zenith angles for this point.  Don't
   ; worry about the matcher offsets here - sensitivity is low.
   ;----------------------------------------------------------------

   cos_cam_zen_angs = COS(REFORM(CamZenithAng[*,ipt]) / !RADEG)
   cos_sun_zen_ang  = COS(SunZenithAng[ipt] / !RADEG)

   ;----------------------------------------------------------------
   ; Loop over the bands and cameras.
   ;----------------------------------------------------------------

   FOR iband=0,3 DO BEGIN

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

         IF ((CamZenithAng[icam,ipt] LT -9000.D) OR $
            (SunZenithAng[ipt] LT -9000.D)) THEN CONTINUE

         ;----------------------------------------------------------
         ; Get the BRFs for this point and band from RawImage for
         ; all cameras. Band order is red, green blue, NIR.
         ; Note the BRFs are at 0.275 km resolution - 1.1 km channel
         ; data are interpolated w/ assistance from the red band.
         ;----------------------------------------------------------

         IF (new_offsets[0,icam,ipt] GT -999.) THEN BEGIN
            brfs[icam] = GetRawImage(icross-new_offsets[0,icam,ipt], $
                                     icross-new_offsets[0,icam,ipt], $
                                     ialong+new_offsets[1,icam,ipt], $
                                     ialong+new_offsets[1,icam,ipt], $
                                     iband, icam, $
                                     !KON.Instr.HI_RES_PIX_SIZE, $
                                     !KON.Misc.INTERP_TYPE_SAMP)
         ENDIF ELSE BEGIN
            brfs[icam] = !KON.Misc.BADVALUE_REAL
         ENDELSE
      ENDFOR

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

         ;----------------------------------------------------------
         ; Compute solid-angle-weighted albedo for this point and
         ; band for each camera.
         ;----------------------------------------------------------

         alb_cam[icam] = !KON.Misc.BADVALUE_REAL

         goodndxs = WHERE(brfs GE 0.0, numgood)
         IF (numgood LT 3) THEN CONTINUE

         SolidAngleAlbedo, icam, 9, cos_cam_zen_angs, brfs, alb_cam, retval

      ENDFOR

      badndxs = WHERE(alb_cam LT 0.0, numbad)
      IF (numbad GT 0) THEN BEGIN
         NB_Albedos[band_index[iband],ipt] = !KON.Misc.BADVALUE_REAL
      ENDIF ELSE BEGIN
         NB_Albedos[band_index[iband],ipt] = TOTAL(alb_cam)
      ENDELSE

   ENDFOR

   ;----------------------------------------------------------------
   ; Compute the broadband albedo for this point.
   ;----------------------------------------------------------------

   alb_temp = NB_Albedos[*,ipt]
   goodndxs = WHERE(alb_temp GT 0.0, numgood)
   IF (numgood LT 4) THEN CONTINUE

   BroadBandAlbedo, cos_sun_zen_ang, NB_Albedos[*,ipt], bb_alb, retval
   BB_Albedos[ipt] = bb_alb

ENDFOR

brfs     = 0
alb_cam  = 0
alb_temp = 0
Offsets  = 0
ndxs     = 0
ndxgood  = 0
goodndxs = 0
badndxs  = 0
new_offsets = 0
cos_cam_zen_angs = 0
cos_sun_zen_ang  = 0

END  ;  ComputeAlbedos

;************************************************************************
PRO ShowAerosolBarChart, RgnName, iBand, TauStruct, AngExpStruct, $
                         SsaStruct, TaufracStruct, Retval
;************************************************************************
; Construct bar charts of tau, angstrom exponent and SSA for the region.
;-------------------------------------------------------------------

COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;-------------------------------------------------------------------
; Get the number of window panes needed to plot the data and set up
; the window and pane sizes.
;-------------------------------------------------------------------

num_panes = 4

size_x = 550
size_y = 500

pos_x  = 400
pos_y  = 100

;-------------------------------------------------------------------
; Set up plot annotation.
;-------------------------------------------------------------------

titles = ['Optical Depth by Band', 'Angstrom Exponent', $
          'Single-Scatter Albedo by Band', 'AOD Fraction by Particle Type']
x_axis_title = ['AOD', 'Angstrom Exponent', 'SSA', 'Green-Band AOD Fraction']
y_axis_title = ['', '', '', '']

;-------------------------------------------------------------------
; Set up the window panes and margins.
;-------------------------------------------------------------------

save_multi = !P.MULTI
!P.MULTI = [0,2,2]

!X.OMARGIN = [0.5,0.5]
!Y.OMARGIN = [2,2]

!X.MARGIN = [6,3]
!Y.MARGIN = [2.5,3]

;-------------------------------------------------------------------
; Create the window to draw profiles in.
;-------------------------------------------------------------------

WINDOW, XSIZE=size_x, YSIZE=size_y, XPOS=pos_x, YPOS=pos_y, $
        RETAIN=2, TITLE='Aerosol Histograms', /FREE

WndwAerosolHist[iBand] = !D.WINDOW

;-------------------------------------------------------------------
; Compute first bin value and number of bins for optical depth and
; draw the histograms.
;-------------------------------------------------------------------

bin_beg = TauStruct.BinNum-1
bin_end = 0

FOR iary=0,TauStruct.MaxInstance-1 DO BEGIN
   IF (TauStruct.ShowAry[iary] EQ 1) THEN BEGIN
      ndxs = WHERE(TauStruct.HistData[*,iary] GT 0, numndxs)
      IF (numndxs GT 0) THEN BEGIN
         IF (MIN(ndxs) LT bin_beg) THEN bin_beg = MIN(ndxs)
         IF (MAX(ndxs) GT bin_end) THEN bin_end = MAX(ndxs)
      ENDIF
      ndxs = 0
   ENDIF
ENDFOR

bin_beg = 0  ;  always start at first Tau = 0.0
numbin = (bin_end - bin_beg + 1) > (TauStruct.BinNum / 2)
bin_end = bin_beg + numbin - 1

IF (numbin GT 0) THEN $
   DrawBarChart, titles[0], x_axis_title[0], y_axis_title[0], $
                 TauStruct, bin_beg, bin_end, numbin, color_ary, $
                 '4.2', retval

;-------------------------------------------------------------------
; Compute first bin value and number of bins for angstrom exponent
; and draw the histogram.
;-------------------------------------------------------------------

bin_beg = AngExpStruct.BinNum-1
bin_end = 0

FOR iary=0,AngExpStruct.MaxInstance-1 DO BEGIN
   IF (AngExpStruct.ShowAry[iary] EQ 1) THEN BEGIN
      ndxs = WHERE(AngExpStruct.HistData[*,iary] GT 0, numndxs)
      IF (numndxs GT 0) THEN BEGIN
         IF (MIN(ndxs) LT bin_beg) THEN bin_beg = MIN(ndxs)
         IF (MAX(ndxs) GT bin_end) THEN bin_end = MAX(ndxs)
      ENDIF
      ndxs = 0
   ENDIF
ENDFOR

numbin = bin_end - bin_beg + 1
IF (numbin LT 10) THEN BEGIN
   bin_beg -= 5
   bin_end += 5
   IF (bin_beg LT 0) THEN BEGIN
      bin_end -= bin_beg
      bin_beg = 0
   ENDIF
   IF (bin_end GT AngExpStruct.BinNum-1) THEN BEGIN
      bin_beg -= (bin_end - AngExpStruct.BinNum)
      bin_end = AngExpStruct.BinNum-1
      IF (bin_beg LT 0) THEN bin_beg = 0
   ENDIF
ENDIF
numbin = bin_end - bin_beg + 1

IF (numbin GT 0) THEN $
   DrawBarChart, titles[1], x_axis_title[1], y_axis_title[1], $
                 AngExpStruct, bin_beg, bin_end, numbin, color_ary, $
                 '5.1', retval

;-------------------------------------------------------------------
; Compute first bin value and number of bins for single-scatter
; albedo and draw the histograms.
;-------------------------------------------------------------------

bin_beg = SsaStruct.BinNum-1
bin_end = 0

numbin = SsaStruct.BinNum
bin_beg = 0
bin_end = SsaStruct.BinNum - 1

IF (numbin GT 0) THEN $
   DrawBarChart, titles[2], x_axis_title[2], y_axis_title[2], $
                 SsaStruct, bin_beg, bin_end, numbin, color_ary, $
                 '4.2', retval

;-------------------------------------------------------------------
; Compute first bin value and number of bins for tau fraction data
; and draw the histograms.
;-------------------------------------------------------------------

bin_beg = TaufracStruct.BinNum-1
bin_end = 0

numbin = TaufracStruct.BinNum
bin_beg = 0
bin_end = TaufracStruct.BinNum - 1

IF (numbin GT 0) THEN $
   DrawBarChart, titles[3], x_axis_title[3], y_axis_title[3], $
                 TaufracStruct, bin_beg, bin_end, numbin, color_ary, $
                 '4.2', retval

;-------------------------------------------------------------------
; Write a key in the tau fraction pane.
;-------------------------------------------------------------------

old_font = GetFontInfo(0)
SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'10', Face:'bold'}
char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.0

colors = [!KON.Colors.ColorVals[3], !KON.Colors.ColorVals[1], $
          !KON.Colors.ColorVals[0], !KON.Colors.ColorVals[2]]
yval = MAX(TaufracStruct.HistData) * 0.94 * 1.1

XYOUTS, 0.10, yval, 'Small',     ALIGNMENT=0.5, COLOR=colors[0]
XYOUTS, 0.30, yval, 'Medium',    ALIGNMENT=0.5, COLOR=colors[1]
XYOUTS, 0.52, yval, 'Large',     ALIGNMENT=0.5, COLOR=colors[2]
XYOUTS, 0.86, yval, 'Spherical', ALIGNMENT=0.5, COLOR=colors[3]

;-------------------------------------------------------------------
; Write a title above all the panes.
;-------------------------------------------------------------------

SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5

XYOUTS, size_x/2, size_y*0.96, 'Histograms for Region: ' + RgnName, $
        ALIGNMENT=0.5, COLOR=0, /DEVICE, CHARSIZE=char_spac

SetFontInfo, old_font

;-------------------------------------------------------------
; Clean up.
;-------------------------------------------------------------

!P.MULTI = save_multi
!X.OMARGIN = [0,0]
!Y.OMARGIN = [0,0]

;-------------------------------------------------------------------
; Save the image to file if requested.
;-------------------------------------------------------------------

IF (!KON.SaveTyp.SAVE_AERHIST_IMAGE) THEN BEGIN
   WriteChartFile, RgnName, 'AerosolHist'
ENDIF

Retval = 0

END  ;  ShowAerosolBarChart

;************************************************************************
PRO DisplayAerosolData, State, CoordStruct, pRgn, iBand, NumPts, $
                        MisrCoords, SomCoords, LonLatCoords, $
                        DrawAerosol, Retval
;************************************************************************
; Display aerosols as histograms and save the histogram data in the
; linked list for this region object.
;------------------------------------------------------------------------

COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON data_structs, region_data, linept_data

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Load aerosol data for all the passed in points.
;------------------------------------------------------------------------
IF (~ !SAV.Digitize.AS_22_OR_23) THEN BEGIN
  GetPgeAerosolData, State, CoordStruct, NumPts, MisrCoords, AerTau_BE, $
                     AerTau_LR, AerAngexp_BE, AerAngexp_LR, AerSsa, $
                     AerTauFrac, retval
  
  IF (retval LT 0) THEN BEGIN
     mssg = 'Could not load AEROSOL standard product data'
     rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
     RETURN
  ENDIF
ENDIF ELSE BEGIN
  GetPgeAerosolV23Data, State, CoordStruct, NumPts, MisrCoords, AerTau_BE, $
    AerAngexp_BE, AerSsa, AerTauFrac, retval

  IF (retval LT 0) THEN BEGIN
    mssg = 'Could not load AEROSOL V23 standard product data'
    rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
    RETURN
  ENDIF
ENDELSE

;------------------------------------------------------------------------
; Store the aerosol data for the digitized points in the point
; objects of the linked list for the appropriate region.
;------------------------------------------------------------------------

ipts = 0
pNext = (*pRgn).pNextLinePt

WHILE PTR_VALID(pNext) DO BEGIN
   (*((*pNext).pData)).aer_tau[*]     = AerTau_BE[*,ipts]
   (*((*pNext).pData)).aer_ssa[*]     = AerSsa[*,ipts]
   (*((*pNext).pData)).aer_taufrac[*] = AerTauFrac[*,ipts]
   (*((*pNext).pData)).aer_angexp     = AerAngexp_BE[ipts]
   ipts += 1
   pNext = (*pNext).pNextSib
ENDWHILE

;------------------------------------------------------------------------
; Construct a histogram of aerosol data for the region and store the
; histogram data in the region object of the linked list.
;------------------------------------------------------------------------

rgn_name = (*((*pRgn).pData)).name[iBand]

GetArrayMinMax, AerTau_BE[0,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val0, max_val0, status
GetArrayMinMax, AerTau_BE[1,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val1, max_val1, status
GetArrayMinMax, AerTau_BE[2,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val2, max_val2, status
GetArrayMinMax, AerTau_BE[3,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val3, max_val3, status

min_val = min_val0 < min_val1 < min_val2 < min_val3
max_val = max_val0 > max_val1 > max_val2 > max_val3
IF (min_val EQ !KON.Misc.LARGE_POS_NUM) THEN min_val = 0.0
IF (max_val EQ !KON.Misc.LARGE_NEG_NUM) THEN max_val = 1.0

InitHistogram, !KON.Histo.TAU_HIST, rgn_name, 0, 4, $
               !KON.Histo.TAU_BIN_CNTR, !KON.Histo.TAU_BIN_SIZE, $
               min_val, max_val, TauStruct

BuildHistogram, 'AER_File - blue',  0, 1, AerTau_BE[0,*], TauStruct
BuildHistogram, 'AER_File - green', 1, 1, AerTau_BE[1,*], TauStruct
BuildHistogram, 'AER_File - red',   2, 1, AerTau_BE[2,*], TauStruct
BuildHistogram, 'AER_File - nir',   3, 1, AerTau_BE[3,*], TauStruct

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   LLStoreHistogram, TauStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tau0_binsize = BinSize
   (*((*pRgn).pData)).tau0_numbins = NumBins
   (*((*pRgn).pData)).tau0_cntr    = pCntrAry
   (*((*pRgn).pData)).tau0_hist    = pHistAry

   LLStoreHistogram, TauStruct, 1, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tau1_binsize = BinSize
   (*((*pRgn).pData)).tau1_numbins = NumBins
   (*((*pRgn).pData)).tau1_cntr    = pCntrAry
   (*((*pRgn).pData)).tau1_hist    = pHistAry

   LLStoreHistogram, TauStruct, 2, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tau2_binsize = BinSize
   (*((*pRgn).pData)).tau2_numbins = NumBins
   (*((*pRgn).pData)).tau2_cntr    = pCntrAry
   (*((*pRgn).pData)).tau2_hist    = pHistAry

   LLStoreHistogram, TauStruct, 3, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tau3_binsize = BinSize
   (*((*pRgn).pData)).tau3_numbins = NumBins
   (*((*pRgn).pData)).tau3_cntr    = pCntrAry
   (*((*pRgn).pData)).tau3_hist    = pHistAry
ENDIF

GetArrayMinMax, AerAngexp_BE, !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val, max_val, status
IF (min_val EQ !KON.Misc.LARGE_POS_NUM) THEN min_val = 0.0
IF (max_val EQ !KON.Misc.LARGE_NEG_NUM) THEN max_val = 1.0

InitHistogram, !KON.Histo.ANGEXP_HIST, rgn_name, 0, 1, $
               !KON.Histo.ANG_BIN_CNTR, !KON.Histo.ANG_BIN_SIZE, $
               min_val, max_val, AngExpStruct

BuildHistogram, 'AER_File', 0, 1, AerAngExp_BE, AngExpStruct
GetArrayMinMax, AerSsa[0,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val0, max_val0, status
GetArrayMinMax, AerSsa[1,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val1, max_val1, status
GetArrayMinMax, AerSsa[2,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val2, max_val2, status
GetArrayMinMax, AerSsa[3,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val3, max_val3, status

min_val = min_val0 < min_val1 < min_val2 < min_val3
max_val = max_val0 > max_val1 > max_val2 > max_val3

InitHistogram, !KON.Histo.SSA_HIST, rgn_name, 0, 4 , $
               !KON.Histo.SSA_BIN_CNTR, !KON.Histo.SSA_BIN_SIZE, $
               0.8, 1.0, SsaStruct

BuildHistogram, 'AER_File - blue',  0, 1, AerSsa[0,*], SsaStruct
BuildHistogram, 'AER_File - green', 1, 1, AerSsa[1,*], SsaStruct
BuildHistogram, 'AER_File - red',   2, 1, AerSsa[2,*], SsaStruct
BuildHistogram, 'AER_File - nir',   3, 1, AerSsa[3,*], SsaStruct

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   LLStoreHistogram, AngExpStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).ang_binsize = BinSize
   (*((*pRgn).pData)).ang_numbins = NumBins
   (*((*pRgn).pData)).angexp_cntr = pCntrAry
   (*((*pRgn).pData)).angexp_hist = pHistAry

   LLStoreHistogram, SsaStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).ssa0_binsize = BinSize
   (*((*pRgn).pData)).ssa0_numbins = NumBins
   (*((*pRgn).pData)).ssa0_cntr    = pCntrAry
   (*((*pRgn).pData)).ssa0_hist    = pHistAry

   LLStoreHistogram, SsaStruct, 1, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).ssa1_binsize = BinSize
   (*((*pRgn).pData)).ssa1_numbins = NumBins
   (*((*pRgn).pData)).ssa1_cntr    = pCntrAry
   (*((*pRgn).pData)).ssa1_hist    = pHistAry

   LLStoreHistogram, SsaStruct, 2, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).ssa2_binsize = BinSize
   (*((*pRgn).pData)).ssa2_numbins = NumBins
   (*((*pRgn).pData)).ssa2_cntr    = pCntrAry
   (*((*pRgn).pData)).ssa2_hist    = pHistAry

   LLStoreHistogram, SsaStruct, 3, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).ssa3_binsize = BinSize
   (*((*pRgn).pData)).ssa3_numbins = NumBins
   (*((*pRgn).pData)).ssa3_cntr    = pCntrAry
   (*((*pRgn).pData)).ssa3_hist    = pHistAry
ENDIF

GetArrayMinMax, AerTauFrac[0,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val0, max_val0, status
GetArrayMinMax, AerTauFrac[1,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val1, max_val1, status
GetArrayMinMax, AerTauFrac[2,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val2, max_val2, status
GetArrayMinMax, AerTauFrac[3,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val3, max_val3, status
GetArrayMinMax, AerTauFrac[4,*], !KON.Misc.LARGE_POS_NUM, $
                !KON.Misc.LARGE_NEG_NUM, min_val4, max_val4, status

min_val = min_val0 < min_val1 < min_val2 < min_val3 < min_val4
max_val = max_val0 > max_val1 > max_val2 > max_val3 > max_val4

InitHistogram, !KON.Histo.TAUFRAC_HIST, rgn_name, 0, 4, $
               !KON.Histo.TFR_BIN_CNTR, !KON.Histo.TFR_BIN_SIZE, $
               0.0, 1.0, TaufracStruct

BuildHistogram, 'AER_File - small',     0, 1, AerTaufrac[0,*], TaufracStruct
BuildHistogram, 'AER_File - medium',    1, 1, AerTaufrac[1,*], TaufracStruct
BuildHistogram, 'AER_File - large',     2, 1, AerTaufrac[2,*], TaufracStruct
BuildHistogram, 'AER_File - spherical', 3, 1, AerTaufrac[3,*], TaufracStruct

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   LLStoreHistogram, TaufracStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tfr0_binsize = BinSize
   (*((*pRgn).pData)).tfr0_numbins = NumBins
   (*((*pRgn).pData)).tfr0_cntr    = pCntrAry
   (*((*pRgn).pData)).tfr0_hist    = pHistAry

   LLStoreHistogram, TaufracStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tfr1_binsize = BinSize
   (*((*pRgn).pData)).tfr1_numbins = NumBins
   (*((*pRgn).pData)).tfr1_cntr    = pCntrAry
   (*((*pRgn).pData)).tfr1_hist    = pHistAry

   LLStoreHistogram, TaufracStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tfr2_binsize = BinSize
   (*((*pRgn).pData)).tfr2_numbins = NumBins
   (*((*pRgn).pData)).tfr2_cntr    = pCntrAry
   (*((*pRgn).pData)).tfr2_hist    = pHistAry

   LLStoreHistogram, TaufracStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
   (*((*pRgn).pData)).tfr3_binsize = BinSize
   (*((*pRgn).pData)).tfr3_numbins = NumBins
   (*((*pRgn).pData)).tfr3_cntr    = pCntrAry
   (*((*pRgn).pData)).tfr3_hist    = pHistAry
ENDIF

;------------------------------------------------------------------------
; Display histograms (bar charts) of aerosol values for the region.
;------------------------------------------------------------------------

IF (DrawAerosol) THEN $
   ShowAerosolBarChart, rgn_name, iBand, TauStruct, AngExpStruct, $
                        SsaStruct, TaufracStruct, retval

;------------------------------------------------------------------------
; Free the histogram structure data and clean up.
;------------------------------------------------------------------------

FreeHistStruct, TauStruct
FreeHistStruct, AngExpStruct
FreeHistStruct, SsaStruct
FreeHistStruct, TaufracStruct

AerTau_BE = 0
AerTau_LR = 0
AerAngexp_BE = 0
AerAngexp_LR = 0
AerSsa = 0
AerTaufrac = 0

Retval = 0

END  ;  DisplayAerosolData
