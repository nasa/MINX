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

;************************************************************************
FUNCTION ComputeGeocentricRadius, Latitude
;************************************************************************
; Compute the radius of the earth at a given geodetic latitude. Input
; values are in degrees.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

latitude_rad = Latitude / !KON.Misc.DoubleRadeg

aa = !KON.Misc.EarthEquatorRadius
bb = !KON.Misc.EarthPolarRadius

sin_lat = SIN(latitude_rad)
cos_lat = COS(latitude_rad)

numer = (aa * aa * cos_lat) ^ 2 + (bb * bb * sin_lat) ^ 2
denom = (aa * cos_lat) ^ 2 + (bb * sin_lat) ^ 2

LocalRadius = SQRT(numer / denom)

RETURN, LocalRadius
   
END  ;  ComputeGeocentricRadius

;************************************************************************
PRO ComputeZenithComponents, CamAzimuth, CamZenith, NumPtsLine, $
                             NorthToSomAzim, NorthToSwathAzim, $
                             CamZenCrossSom, CamZenAlongSom, $
                             CamZenCrossSwath, CamZenAlongSwath, Retval
;************************************************************************
; Compute the along-track and across-track components of camera zenith
; angles, both relative to SOM north and SWATH north (this eliminates the
; camera azimuth angle). MISR Camera Zenith is the angle between the -z
; axis and the camera vector. Values range between 0 and 90 degrees (all
; positive!). MISR Camera Azimuth is the angle measured clockwise from
; the local north vector to the projection onto the x,y plane of the
; camera vector. Values range between 0 and 360 degrees. The MISR Camera
; Vector is anchored at the ground point and is directed toward the
; camera in the direction of photon travel. However, MINX reversed this
; direction when the geometry data were loaded.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

CamZenCrossSom   = DBLARR(!KON.Instr.NCAM, NumPtsLine) + $
                          !KON.Misc.BADVALUE_REAL
CamZenAlongSom   = DBLARR(!KON.Instr.NCAM, NumPtsLine) + $
                          !KON.Misc.BADVALUE_REAL
CamZenCrossSwath = DBLARR(!KON.Instr.NCAM, NumPtsLine) + $
                          !KON.Misc.BADVALUE_REAL
CamZenAlongSwath = DBLARR(!KON.Instr.NCAM, NumPtsLine) + $
                          !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Mark which cameras have good and bad data.
;------------------------------------------------------------------------

ndxgood = WHERE(CamAzimuth NE !KON.Misc.BADVALUE_REAL AND $
                CamZenith  NE !KON.Misc.BADVALUE_REAL AND $
                NorthToSomAzim   NE !KON.Misc.BADVALUE_REAL AND $
                NorthToSwathAzim NE !KON.Misc.BADVALUE_REAL, numgood, $
                COMPLEMENT=ndxbad, NCOMPLEMENT=numbad)

IF (numgood EQ 0) THEN RETURN

;------------------------------------------------------------------------
; Initialize temporary variables.
;------------------------------------------------------------------------

cam_azimuth = CamAzimuth[ndxgood]
cam_zenith  = CamZenith[ndxgood]

ntosom_azimuth = NorthToSomAzim[ndxgood]
ntoswa_azimuth = NorthToSwathAzim[ndxgood]

;------------------------------------------------------------------------
; Compute the angle clockwise from SOM north to the azimuth of the
; comparison camera. Convert degrees to radians.
;------------------------------------------------------------------------

som_to_cam_azim = (cam_azimuth - ntosom_azimuth) / !KON.Misc.DoubleRadeg

;------------------------------------------------------------------------
; Compute the components of the cameras' zenith angles projected to the
; SOM across and along directions. This zenith pair replaces the cam-
; zenith / cam-azimuth pair.
;------------------------------------------------------------------------

CamZenCrossSom[ndxgood] = cam_zenith * SIN(som_to_cam_azim)
CamZenAlongSom[ndxgood] = cam_zenith * COS(som_to_cam_azim)

;------------------------------------------------------------------------
; Compute the angle clockwise from SWATH north to the azimuth of the
; comparison camera. Convert degrees to radians.
;------------------------------------------------------------------------

swath_to_cam_azim = (cam_azimuth - ntoswa_azimuth) / !KON.Misc.DoubleRadeg

;------------------------------------------------------------------------
; Compute the components of the cameras' zenith angles projected to the
; SWATH across and along directions. This zenith pair replaces the cam-
; zenith / cam-azimuth pair.
;------------------------------------------------------------------------

CamZenCrossSwath[ndxgood] = cam_zenith * SIN(swath_to_cam_azim)
CamZenAlongSwath[ndxgood] = cam_zenith * COS(swath_to_cam_azim)

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

cam_azimuth = 0
cam_zenith  = 0
ntosom_azimuth = 0
ntoswa_azimuth = 0
som_to_cam_azim = 0
swath_to_cam_azim = 0
ndxgood = 0
ndxbad = 0
ndxs = 0

Retval = 0

END  ;  ComputeZenithComponents

;************************************************************************
PRO RotateDirectionLine, DirecSlopeSom, DirecSlopeSwath, DirecSomN, $
                         DirecSwathN, Retval
;************************************************************************
; Transform the slope at points on the direction line from orthogonal to
; SOM direction to orthogonal to swath direction. SomToSwathAngles are
; the angles between the SOM north and the swath north directions for
; each camera. A positive value means the SWATH direction is clockwise
; of the SOM direction. Also rotate the angular direction (0-360).
;------------------------------------------------------------------------

COMMON blktimeangle, BlockCntrTime, SomToSwathAngles

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

DirecSlopeSwath = DirecSlopeSom * 0.0 + !KON.Misc.BADVALUE_REAL
num_slope = N_ELEMENTS(DirecSlopeSom)

;------------------------------------------------------------------------
; Get the SOM-relative components of the slope.
;------------------------------------------------------------------------

dy = DOUBLE(DirecSlopeSom)
dx = DBLARR(num_slope) * 0.0 + 1.0

;------------------------------------------------------------------------
; Rotate the slopes. Use the An camera's swath direction for all cameras.
; The max difference between using the An camera and the average of the
; An and other camera is generally less than 1 degree.
;------------------------------------------------------------------------

avg_swath_ang = SomToSwathAngles[!KON.Instr.AN]

sin_ang = SIN(avg_swath_ang)
cos_ang = COS(avg_swath_ang)

comp_cross_swath = dx * cos_ang - dy * sin_ang
comp_along_swath = dx * sin_ang + dy * cos_ang

;------------------------------------------------------------------------
; Convert the components back to slopes and save the rotated values in
; the return parameter. Note that slopes do not preserve the quadrant the
; direction line points to. Protect against large slopes!
;------------------------------------------------------------------------

min_val = 1.0 / !KON.Misc.SLOPE_MAX_THRESH1
ndxgood = WHERE(ABS(comp_cross_swath) GE min_val, numndxgood, COMPLEMENT=ndxbad, $
                NCOMPLEMENT=numndxbad)

IF (numndxbad GT 0) THEN BEGIN
   comp_cross_swath[ndxbad] = min_val
   sign_bad = comp_cross_swath[ndxbad] / ABS(comp_cross_swath[ndxbad])
   DirecSlopeSwath[ndxbad] = !KON.Misc.SLOPE_MAX_THRESH2 * sign_bad
ENDIF

IF (numndxgood GT 0) THEN BEGIN
   temp_slope = comp_along_swath[ndxgood] / comp_cross_swath[ndxgood]
   ndxbad = WHERE(ABS(temp_slope) GT !KON.Misc.SLOPE_MAX_THRESH2, numndxbad)
   IF (numndxbad GT 0) THEN $
      temp_slope[ndxbad] = !KON.Misc.SLOPE_MAX_THRESH2 * $
                           temp_slope[ndxbad] / ABS(temp_slope[ndxbad])
   DirecSlopeSwath[ndxgood] = temp_slope
   temp_slope = 0
ENDIF

;------------------------------------------------------------------------
; Rotate the direction angles.
;------------------------------------------------------------------------

DirecSwathN = DirecSomN - SomToSwathAngles[!KON.Instr.AN] * !RADEG

ndxbad = WHERE(DirecSwathN LT 0.0, numndxbad)
IF (numndxbad GT 0) THEN DirecSwathN[ndxbad] += 360.0

ndxbad = WHERE(DirecSwathN GT 360.0, numndxbad)
IF (numndxbad GT 0) THEN DirecSwathN[ndxbad] -= 360.0

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

dx = 0
dy = 0
sign_bad = 0
comp_cross_swath = 0
comp_along_swath = 0
ndxgood = 0
ndxbad = 0

Retval = 0

END  ;  RotateDirectionLine

;************************************************************************
PRO RotateDisparityComponents, DisparitySom, DisparitySwath, Retval
;************************************************************************
; Transform the along-track and across-track components of disparity
; from SOM coordinates to SWATH coordinates to match zenith angle along
; and across components.
; SomToSwathAngles are the angles between the SOM north and the SWATH north
; directions for each camera. A negative value means the SWATH direction
; is clockwise of the SOM direction.
;------------------------------------------------------------------------

COMMON blktimeangle, BlockCntrTime, SomToSwathAngles

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

DisparitySwath = DisparitySom * 0.0 + !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Loop over the cameras. Skip the An camera.
;------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   IF (icam EQ !KON.Instr.AN) THEN CONTINUE

   ;---------------------------------------------------------------------
   ; Mark which points have good data for this camera.
   ;---------------------------------------------------------------------

   disp_cross = REFORM(DisparitySom[0,icam,*])
   disp_along = REFORM(DisparitySom[1,icam,*])

   ndxgood = WHERE(disp_cross NE !KON.Misc.BADVALUE_REAL AND $
                   disp_along NE !KON.Misc.BADVALUE_REAL, numgood)

   IF (numgood EQ 0) THEN CONTINUE

   ;---------------------------------------------------------------------
   ; Select out only the valid disparities and cameras.
   ;---------------------------------------------------------------------

   disp_cross_valid = disp_cross[ndxgood]
   disp_along_valid = disp_along[ndxgood]

   ;---------------------------------------------------------------------
   ; Rotate the vector by the angle between SOM north and Swath north.
   ; The swath angle for disparities is the average of the comparison and
   ; reference cameras. Changed to just be the An camera angle.
   ;---------------------------------------------------------------------

   avg_swath_ang = SomToSwathAngles[!KON.Instr.AN]

   disp_cross_swath = disp_cross_valid * COS(avg_swath_ang) - $
                      disp_along_valid * SIN(avg_swath_ang)
   
   disp_along_swath = disp_cross_valid * SIN(avg_swath_ang) + $
                      disp_along_valid * COS(avg_swath_ang)

   ;---------------------------------------------------------------------
   ; Rotate the valid disparity vectors by the swath angle and save the
   ; rotated values in the return parameter. 
   ;---------------------------------------------------------------------

   disp_cross[ndxgood] = disp_cross_swath
   disp_along[ndxgood] = disp_along_swath

   DisparitySwath[0,icam,*] = disp_cross
   DisparitySwath[1,icam,*] = disp_along

ENDFOR

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

disp_cross = 0
disp_along = 0
disp_cross_valid = 0
disp_along_valid = 0
disp_cross_swath = 0
disp_along_swath = 0
ndxgood = 0
ndxbad = 0

Retval = 0

END  ;  RotateDisparityComponents

;************************************************************************
PRO FindBestValue, Type, CamValuesIn, CamCorrCoeffs, CamNdxs, NumCamsReq, $
                   BestValueOut, NewNdxGood, NoMorePlots
;************************************************************************
; Given a set of heights (or winds) for N cameras, find a single value,
; the consensus value, that represents the best estimate of the actual
; value. This is a poor man's cluster analysis: find the largest cluster
; of values whose data range is less than the acceptance width.
; Type: the type of data passed in can be
;       1 => zero-wind ht;
;       2 => wind-corr ht;
;       3 => across-track wind;
;       4 => along-track wind.
; Correlation coefficients are for each cameras image-matching result -
; camera retrievals with better image correlations are weighted more.
; CamNdxs are the camera indices (0=Df => 8=Da) corresponding to the order
; of the input value and correlation coefficient indexes.
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; Initialize parameters.
; Set next to 0 for production
;------------------------------------------------------------------------
SHOW_CAM_DATA = 0  ; This is a useful diagnostic for testing.
                   ; 0 = don't show hts and winds;
                   ; 1 = show heights and winds;
                   ; 2 = show heights only;
                   ; 3 = show winds only

BestValueOut = !KON.Misc.BADVALUE_REAL
NewNdxGood = []

NumCams = N_ELEMENTS(CamValuesIn)
IF (NumCams LT NumCamsReq) THEN RETURN

FactorRelaxed = (!SAV.Digitize.RELAX_THRESH EQ 1) ? 2.0 : 1.0
Factor2Cams = 0.5
FactorWeights = 2.0
FactorAddRange = 0.35

;------------------------------------------------------------------------
; Define the acceptance width - the maximum distance apart that camera
; retrieval values can be for the values that lie within that range to be
; accepted. This width is a linear function of heights or winds.
;------------------------------------------------------------------------

IF (Type LE 2) THEN BEGIN
   LinearA = (Type EQ 1) ? !SAV.HtWind.ZR_HGT_THRESH_MIN : $
                           !SAV.HtWind.WC_HGT_THRESH_MIN
   LinearB = (Type EQ 1) ? !SAV.HtWind.ZR_HGT_THRESH_SLOPE : $
                           !SAV.HtWind.WC_HGT_THRESH_SLOPE
ENDIF ELSE BEGIN
   LinearA = !SAV.HtWind.WC_WND_THRESH_MIN
   LinearB = !SAV.HtWind.WC_WND_THRESH_SLOPE
ENDELSE

AcceptWidth = (LinearA + LinearB * MEAN(ABS(CamValuesIn))) * FactorRelaxed

;------------------------------------------------------------------------
; If fewer than 2 points are available, return the bad value indicator.
; If only 2 points are available, and their separation is less than
; AcceptWidth m of each other for height (AcceptWidth m/s for wind), then
; return their mean as the best estimate. Because only 2 points are
; available, make AcceptWidth much stricter.
;------------------------------------------------------------------------

IF (NumCams LT NumCamsReq) THEN RETURN

IF (NumCams EQ 2) THEN BEGIN
   AcceptWidth2Cams = AcceptWidth * Factor2Cams
   IF (ABS(CamValuesIn[0] - CamValuesIn[1]) LE AcceptWidth2Cams) THEN $
      BestValueOut = (CamValuesIn[0] + CamValuesIn[1]) / 2.0
   RETURN
ENDIF

;------------------------------------------------------------------------
; Create a data structure to contain the camera-indexed parameters which
; will all be sorted at once: retrieval values, camera indexes themselves
; and image matcher correlation coefficients.
;------------------------------------------------------------------------

camera_data = { cam_value : 0.0, cam_number : 0, corr_coeff : 0.0 }
cam_data = REPLICATE(camera_data, NumCams)

FOR icam=0,NumCams-1 DO BEGIN
   cam_data[icam].cam_value  = CamValuesIn[icam]
   cam_data[icam].cam_number = CamNdxs[icam]
   cam_data[icam].corr_coeff = CamCorrCoeffs[icam]
ENDFOR

;------------------------------------------------------------------------
; Sort the retrieval values in the data structure by increasing aerosol
; height or wind.
;------------------------------------------------------------------------

sort_ndx = SORT(CamValuesIn)
cam_data = cam_data[sort_ndx]

;------------------------------------------------------------------------
; Next form multiple levels of the sorted value ranges. These comprise
; all possible combinations of contiguous, retrieved values.E.g., if 8
; cameras paired with An had successful retrievals, are sorted by their
; values and are spaced as follows:
;       0      1          2  3 4     5          6  7
; Compute these ranges (never more than 28 ranges, typically 10 to 15!):
;   [0->1, 1->2, 2->3, 3->4, 4->5, 5->6, 6->7]  (every 1)
;   [0->2, 1->3, 2->4, 3->5, 4->6, 5->7]        (every 2nd)
;   [0->3, 1->4, 2->5, 3->6, 4->7]              (every 3rd)
;   [0->4, 1->5, 2->6, 3->7]                    (every 4th)
;   [0->5, 1->6, 2->7]                          (every 5th)
;   [0->6, 1->7]                                (every 6th)
;   [0->7]                                      (every 7th)
; The last range [0->7] is the best possible outcome - it means that all
; 8 cameras had successful retrievals and lie within range of acceptance.
; Store the values in a diagonal matrix. Then compute a weight for each
; range entry that is an exponential of the number of values in the range
; plus some integer (2).
;------------------------------------------------------------------------

range_mtrx = FLTARR(NumCams-1,NumCams-1) + !KON.Misc.BADVALUE_REAL
range_wght = FLTARR(NumCams-1,NumCams-1) - !KON.Misc.BADVALUE_REAL
delta_pts = 0

FOR icam1=0,NumCams-2 DO BEGIN   
   delta_pts += 1
   weight_factor = (delta_pts + 2) ^ FactorWeights
   FOR icam2=0,NumCams-icam1-2 DO BEGIN
      range_mtrx[icam2,icam1] = cam_data[icam2+delta_pts].cam_value - $
                                cam_data[icam2].cam_value
      range_wght[icam2,icam1] = range_mtrx[icam2,icam1] / weight_factor
   ENDFOR
ENDFOR

;------------------------------------------------------------------------
; Exit with failure if no range has a valid range_wght.
;------------------------------------------------------------------------

ndx_good = WHERE(range_wght LT !KON.Misc.BADVALUE_REAL * (-1.0), num_good)
IF (num_good EQ 0) THEN RETURN

;------------------------------------------------------------------------
; Find the smallest weight in each row of the weight matrix.
;------------------------------------------------------------------------

min_wghts = FLTARR(NumCams-1) - !KON.Misc.BADVALUE_REAL
min_wght_ndx = INTARR(NumCams-1)
max_wght_ndx = INTARR(NumCams-1)

FOR irow=0,NumCams-2 DO BEGIN
   min_wt = MIN(range_wght[*,irow], min_ndx)
   IF (min_wt EQ !KON.Misc.BADVALUE_REAL * (-1.0)) THEN CONTINUE
   
   min_wghts[irow] = min_wt
   min_wght_ndx[irow] = min_ndx
   max_wght_ndx[irow] = min_wght_ndx[irow] + irow + 1
ENDFOR

;------------------------------------------------------------------------
; Sort the weights into ascending order. Then work from the smallest
; weight to the largest, inspecting each, and stop when and if a range
; is found that is within the separation threshold for this data point.
; The acceptance width is modified dynamically as a function of the
; standard deviation of the heights in each range. Because we want to
; avoid using only 2 cameras, skip the entry in the top level of ranges.
;------------------------------------------------------------------------

minmin_wght = MIN(min_wghts, minmin_ndx)
sort_ndx = SORT(min_wghts)
good_cams = [-1]

FOR irow=0,NumCams-2 DO BEGIN
   jrow = sort_ndx[irow]
   IF (jrow EQ 0) THEN CONTINUE
   row_range = cam_data[max_wght_ndx[jrow]].cam_value - $
               cam_data[min_wght_ndx[jrow]].cam_value > 0.2
   val_stdev = STDDEV(cam_data[min_wght_ndx[jrow]: $
                               max_wght_ndx[jrow]].cam_value) > 0.05
   sd_frac_range = SQRT(val_stdev / row_range)
   val_mean = MEAN(ABS(cam_data[min_wght_ndx[jrow]: $
                                max_wght_ndx[jrow]].cam_value))
   AcceptWidth = (LinearA + LinearB * val_mean) * FactorRelaxed
   accept_range = AcceptWidth * (sd_frac_range + FactorAddRange)   
   IF (row_range GT accept_range) THEN CONTINUE
   
   good_cams = cam_data[min_wght_ndx[jrow]:max_wght_ndx[jrow]].cam_number
   good_vals = cam_data[min_wght_ndx[jrow]:max_wght_ndx[jrow]].cam_value
   good_ccoefs = cam_data[min_wght_ndx[jrow]:max_wght_ndx[jrow]].corr_coeff
ENDFOR

;------------------------------------------------------------------------
; If no other level produced success, then try the top level. If this is
; not successful, then return with failure.
;------------------------------------------------------------------------

IF (good_cams[0] EQ -1) THEN BEGIN
   row_range = ABS(cam_data[max_wght_ndx[0]].cam_value - $
                   cam_data[min_wght_ndx[0]].cam_value)
   val_mean = MEAN(ABS(cam_data[min_wght_ndx[0]: $
                                max_wght_ndx[0]].cam_value))
   AcceptWidth2Cams = (LinearA + LinearB * val_mean) * FactorRelaxed * $
                      Factor2Cams
   IF (row_range GT AcceptWidth2Cams) THEN RETURN
   
   good_cams = cam_data[min_wght_ndx[0]:max_wght_ndx[0]].cam_number
   good_vals = cam_data[min_wght_ndx[0]:max_wght_ndx[0]].cam_value
   good_ccoefs = cam_data[min_wght_ndx[0]:max_wght_ndx[0]].corr_coeff
ENDIF

;------------------------------------------------------------------------
; Weight the selected camera values by the square of their correlation
; coefficients to compute the best estimate of overall value.
;------------------------------------------------------------------------

good_ccoefs *= good_ccoefs
sum_coeffs = TOTAL(good_ccoefs)
BestValueOut = TOTAL(good_vals * good_ccoefs) / sum_coeffs

;------------------------------------------------------------------------
; Display the camera values, correlation coefficients, camera numbers,
; cameras used and output value.
;------------------------------------------------------------------------

IF (SHOW_CAM_DATA GT 0) THEN BEGIN
   IF ((SHOW_CAM_DATA EQ 1) OR (Type LE 2 AND SHOW_CAM_DATA EQ 2) OR $
                               (Type GE 3 AND SHOW_CAM_DATA EQ 3)) THEN $   
      DisplayHtsAndCCoeffs, Type, cam_data, good_cams, range_mtrx, $
                            range_wght, BestValueOut, NoMorePlots
ENDIF

;------------------------------------------------------------------------
; Height data must be converted from meters to km for zero-wind hts.
;------------------------------------------------------------------------

IF (Type LT 2) THEN BestValueOut /= 1000.0
NewNdxGood = good_cams

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------
 
range_mtrx = 0
range_wght = 0
sort_ndx = 0
cam_data = 0
good_cams = 0
good_vals = 0
ndxgood = 0
                 
END  ;  FindBestValue

;************************************************************************
PRO GetZeroWindHeight, RadiusLocal, TerrainElev, ZenithAlong, $
                       DisparityAlong, Hght
;************************************************************************
; Analytically compute zero wind heights for all cameras above ellipsoid
; given the local earth radius of curvature, the terrain elevations at
; the reference and comparison camera feature locations, the reference
; and comparison camera zenith angles at the terrain location of the
; features and the camera disparities. Also return the distance from the
; feature in the An image where the cloud sub-point lies.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Only operate on cameras with valid along-track disparities, terrain
; heights and along-track component of zenith angle.
;------------------------------------------------------------------------

DisparityAlong[!KON.Instr.AN] = !KON.Misc.BADVALUE_REAL

ndxgood = WHERE(DisparityAlong NE !KON.Misc.BADVALUE_REAL AND $
                TerrainElev    NE !KON.Misc.BADVALUE_REAL AND $
                ZenithAlong    NE !KON.Misc.BADVALUE_REAL, numgood)

IF (numgood EQ 0) THEN RETURN

GoodCamNdx = ndxgood

;------------------------------------------------------------------------
; Initialize some values.
;------------------------------------------------------------------------

RefTerrainElev = TerrainElev[!KON.Instr.AN] * 1000.0D
RefZenithAlong = ZenithAlong[!KON.Instr.AN] / !KON.Misc.DoubleRadeg
terrain_elev = TerrainElev[ndxgood] * 1000.0D
zenith_along = ZenithAlong[ndxgood] / !KON.Misc.DoubleRadeg
disparity_along = DisparityAlong[ndxgood] * 1000.0D * $
                  !KON.Instr.HI_RES_PIX_SIZE

;------------------------------------------------------------------------
; Find the angle in radians subtended by the disparities at the terrain
; using the local radius of curvature of the earth. Disparities are
; measured from the reference camera position to the comparison camera
; position. A disparity pointing north is positive. beta has same sign.
;------------------------------------------------------------------------

beta = disparity_along / RadiusLocal

;------------------------------------------------------------------------
; Convert polar to rectangular coordinates for the 2 matched image points
; at the terrain. 
;------------------------------------------------------------------------

X_RefTer = 0.0D
Y_RefTer = RadiusLocal + RefTerrainElev

beta2 = !DPI / 2.0 - beta

RadiusTerrain = RadiusLocal + terrain_elev
X_CmpTer = RadiusTerrain * COS(beta2)
Y_CmpTer = RadiusTerrain * SIN(beta2)

;------------------------------------------------------------------------
; Convert the cam zenith angle to the rectangular coordinate system. For
; pairs with forward-looking cameras, CmpZenithAlong increases (beta is
; negative). For pairs with aft-looking cameras, CmpZenithAlong decreases
; (beta is positive).
;------------------------------------------------------------------------

NewZenithAlong = zenith_along - beta

;------------------------------------------------------------------------
; Extend the look vectors for An and other camera out 100 km and get the
; rectangular coordinates of those points.
;------------------------------------------------------------------------

X_RefPrj = X_RefTer - 100000.0D * SIN(RefZenithAlong)
Y_RefPrj = Y_RefTer + 100000.0D * COS(RefZenithAlong)

X_CmpPrj = X_CmpTer - 100000.0D * SIN(NewZenithAlong)
Y_CmpPrj = Y_CmpTer + 100000.0D * COS(NewZenithAlong)

;------------------------------------------------------------------------
; Find the intersection of the two look vectors - the solution point.
;------------------------------------------------------------------------

X_Sol = $
   ((X_RefTer * Y_RefPrj - Y_RefTer * X_RefPrj) * (X_CmpTer - X_CmpPrj) - $
    (X_RefTer - X_RefPrj) * (X_CmpTer * Y_CmpPrj - Y_CmpTer * X_CmpPrj)) / $
   ((X_RefTer - X_RefPrj) * (Y_CmpTer - Y_CmpPrj) - $
    (Y_RefTer - Y_RefPrj) * (X_CmpTer - X_CmpPrj))

Y_Sol = $
   ((X_RefTer * Y_RefPrj - Y_RefTer * X_RefPrj) * (Y_CmpTer - Y_CmpPrj) - $
    (Y_RefTer - Y_RefPrj) * (X_CmpTer * Y_CmpPrj - Y_CmpTer * X_CmpPrj)) / $
   ((X_RefTer - X_RefPrj) * (Y_CmpTer - Y_CmpPrj) - $
    (Y_RefTer - Y_RefPrj) * (X_CmpTer - X_CmpPrj))

;------------------------------------------------------------------------
; Convert the rectangular coordinates of the intersection back to polar
; coordinates and return the height and offset of the sub-cloud point.
;------------------------------------------------------------------------

IF (numgood GT 0) THEN $
   Hght[ndxgood] = SQRT(X_Sol ^ 2 + Y_Sol ^ 2) - RadiusLocal

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

beta = 0
beta2 = 0
terrain_elev = 0
disparity_along = 0
zenith_along = 0
RadiusTerrain = 0
NewZenithAlong = 0
X_CmpTer = 0
Y_CmpTer = 0
X_CmpPrj = 0
Y_CmpPrj = 0
X_Sol = 0
Y_Sol = 0
ndxgood = 0

END  ;  GetZeroWindHeight

;************************************************************************
PRO ComputeZeroWindHts, NumPts, RadiusLocal, Offsets, CorrCoeffs, $
                        NoMorePlots, TerrainHts, CamZenithAlong, $
                        PtHtsZero_Cam, PtHtsZero, Retval
;************************************************************************
; Use the along and across disparities together with the camera geometry
; to compute the zero-wind stereo heights. Compute heights at each point
; for all available camera pairs and find the best solution. Note that
; all offsets are considered positive if they are north or east, i.e.
; toward the right and top of the swath.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

MinReqCams = 2

IF (!SAV.Digitize.PRINT_OFFSETS EQ 2) THEN $
   PtHtsZero_Cam = FLTARR(!KON.Instr.NCAM, NumPts) + !KON.Misc.BADVALUE_REAL

Hghts_Along = FLTARR(!KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Loop over the points comprising the plume direction line or the points
; in a region.
;------------------------------------------------------------------------

FOR ipts=0,NumPts-1 DO BEGIN

   ;---------------------------------------------------------------------
   ; Only continue with this point if terrain heights, disparities, and
   ; along-track geometry are valid for the required number of cameras.
   ; The terrain and geometry must be valid for the An camera also, but
   ; not disparities. Offsets will eliminate An automatically.
   ;---------------------------------------------------------------------

   GetZeroWindHeight, DOUBLE(RadiusLocal), $
                      DOUBLE(REFORM(TerrainHts[*,ipts])), $
                      DOUBLE(REFORM(CamZenithAlong[*,ipts])), $
                      DOUBLE(REFORM(Offsets[1,*,ipts])), Hghts_Along

   ndxgood = WHERE(Hghts_Along NE !KON.Misc.BADVALUE_REAL AND $
                   Hghts_Along NE 0.0, numgood)

   PtHtsZero[ipts] = !KON.Misc.BADVALUE_REAL

   IF (numgood GE MinReqCams) THEN BEGIN

      ;------------------------------------------------------------------
      ; Determine the best zero-wind ht to report from among successful
      ; camera values.
      ;------------------------------------------------------------------
      
      FindBestValue, 1, Hghts_Along[ndxgood], CorrCoeffs[ndxgood,ipts], $
                     ndxgood, MinReqCams, best_value, new_ndxgood, $
                     NoMorePlots
   
      IF (best_value GT $
          !KON.DataRgn.VALUE_MIN[!KON.DataRgn.TYPE_ZEROWIND_HT] AND $
          best_value LT $
          !KON.DataRgn.VALUE_MAX[!KON.DataRgn.TYPE_ZEROWIND_HT]) THEN $

         PtHtsZero[ipts] = best_value
   ENDIF
   
   ;---------------------------------------------------------------------
   ; Clean up.
   ;---------------------------------------------------------------------

   Hghts_Along[*] = !KON.Misc.BADVALUE_REAL
   ndxgood = 0
   new_ndxgood = 0

ENDFOR

Hghts_Along = 0

END  ;  ComputeZeroWindHts

;************************************************************************
PRO GetBestCameraHtWndFit, Ipts, GoodCamsIn, Slope, WindDirection, $
                           WndCorrHt_Cam, WndCross_Cam, WndAlong_Cam, $
                           TerrainHts, Offsets, CorrCoeffs, NoMorePlots, $
                           GoodCamsOut, WndCorrHt, WndCross, WndAlong, $
                           Retval
;************************************************************************
; New method of computing the best concensus heights and winds from the
; solution lines for the successful cameras.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Initialize the output values.
;------------------------------------------------------------------------

best_value_ht = !KON.Misc.BADVALUE_REAL
best_value_wc = !KON.Misc.BADVALUE_REAL
best_value_wa = !KON.Misc.BADVALUE_REAL
new_ndxgood = []

WndCorrHt = !KON.Misc.BADVALUE_REAL
WndCross = !KON.Misc.BADVALUE_REAL
WndAlong = !KON.Misc.BADVALUE_REAL
GoodCamsOut = 0

;------------------------------------------------------------------------
; If the slope of the direction line at this point is greater than half
; the max threshold of 15 (or an angle of 82 degrees), then the retrieval
; must find at least 3 good camera pair results instead of 2. This helps
; weed out noisy results as the plume direction approaches the swath
; direction.
;------------------------------------------------------------------------

MinReqCams = (ABS(Slope) GE 0.5 * !KON.Misc.SLOPE_MAX_THRESH2) ? 3 : 2

;------------------------------------------------------------------------
; Find the cameras with good values.
;------------------------------------------------------------------------

ndxgood0 = WHERE(WndCorrHt_Cam NE !KON.Misc.BADVALUE_REAL AND $
                 WndAlong_Cam  NE !KON.Misc.BADVALUE_REAL AND $
                 WndCross_Cam  NE !KON.Misc.BADVALUE_REAL, numgood0)

;------------------------------------------------------------------------
; Determine the best wind-corrected height to report from among successful
; camera values. Then get the best winds requiring that the cameras be
; found from among the successful height retrieval cameras. This assures
; that no extraneous values are included and that the ratio of wind along
; to wind across remains constant for all points.
;------------------------------------------------------------------------

IF (numgood0 GE MinReqCams) THEN BEGIN
   FindBestValue, 2, WndCorrHt_Cam[ndxgood0], CorrCoeffs[ndxgood0], ndxgood0, $
                  MinReqCams, best_value_ht, new_ndxgood0, NoMorePlots
   IF (N_ELEMENTS(new_ndxgood0) LT MinReqCams) THEN RETURN
   
   new_ndxgood0 = new_ndxgood0[SORT(new_ndxgood0)]
   FindBestValue, 3, WndCross_Cam[new_ndxgood0], CorrCoeffs[new_ndxgood0], $
                  new_ndxgood0, MinReqCams, best_value_wc, new_ndxgood1, $
                  NoMorePlots
   FindBestValue, 4, WndAlong_Cam[new_ndxgood0], CorrCoeffs[new_ndxgood0], $
                  new_ndxgood0, MinReqCams, best_value_wa, new_ndxgood2, $
                  NoMorePlots
ENDIF

ndxgood0 = 0
new_ndxgood1 = 0
new_ndxgood2 = 0

;------------------------------------------------------------------------
; Height, wind across and wind along must all be retrieved for success.
; And retrieved heights and winds must fall within the user-specified
; range of variability.
;------------------------------------------------------------------------

IF (N_ELEMENTS(new_ndxgood0) LT MinReqCams OR $
    best_value_ht EQ !KON.Misc.BADVALUE_REAL OR $
    best_value_wc EQ !KON.Misc.BADVALUE_REAL OR $
    best_value_wa EQ !KON.Misc.BADVALUE_REAL) THEN RETURN

ht_in_km = best_value_ht / 1000.0
total_wind = SQRT(best_value_wc * best_value_wc + $
                  best_value_wa * best_value_wa)
 
IF (ht_in_km LT (!SAV.Digitize.MIN_HGHT + TerrainHts[!KON.Instr.AN]) OR $
    ht_in_km  GT !SAV.Digitize.MAX_HGHT OR $
   total_wind GT !SAV.Digitize.MAX_WIND) THEN RETURN

;------------------------------------------------------------------------
; If bi-directional retrieval flag is not set, delete the retrieval if
; either component of the retrieved wind vector points to the wrong
; quadrant, relative to that provided by the user.
;------------------------------------------------------------------------

IF (~ !SAV.Digitize.BI_DIR_WIND) THEN BEGIN
   usr_cross = (WindDirection GE  0.0 AND $
                WindDirection LT 180.0) ? 1.0 : (-1.0)
   usr_along = (WindDirection GE 90.0 AND $
                WindDirection LT 270.0) ? (-1.0) : 1.0
   IF (best_value_wa * usr_along LT 0.0 OR $
       best_value_wc * usr_cross LT 0.0) THEN new_ndxgood0 = []
ENDIF

IF (N_ELEMENTS(new_ndxgood0) LT MinReqCams) THEN RETURN

;------------------------------------------------------------------------
; Return retrieved values and a list of the successful cameras. Retval
; must be 0 for this retrieval to be used by caller.
;------------------------------------------------------------------------

WndCorrHt = best_value_ht
WndCross  = best_value_wc
WndAlong  = best_value_wa
GoodCamsOut = new_ndxgood0

new_ndxgood0 = 0

Retval = 0

END  ;  GetBestCameraHtWndFit

;************************************************************************
PRO GetHeightWindSolutionLine, CrOrAl, PtNum, RadiusLocal, TerrainElev, $
                               Zenith, Disparity, NumHts, FeatureHeight, $
                               WindSpeed, HorzOffset, GoodCamNdx, Status
;************************************************************************
; Analytically compute either across-track or along-track wind speed for
; all cameras given local earth radius of curvature, terrain elevations
; at reference and comparison camera terrain disparity locations,
; reference & comparison camera zenith angles at the terrain disparity
; locations, and camera disparities.
; A single value can't be found, so return winds for a range of height
; values that will define the "solution line" for each camera at a point.
; All values are passed in as double precision.
;************************************************************************
;Height/Wind Retrieval Sign Conventions
;
;Camera time difference relative to An:
;   Forward cameras – negative
;   Aftward cameras – positive
;
;Zenith angle:
;   Across – positive if look vector from camera points toward right
;            negative if look vector from camera points toward left
;   Along  – positive if look vector from camera points toward top
;            negative if look vector from camera points toward bottom
;
;Disparity:
;   Across – positive if comparison cam match is right of reference
;            negative if comparison cam match is left of reference
;   Along  – positive if comparison cam match is above reference
;            negative if comparison cam match is below reference
;
;Wind direction:
;   Across – positive if wind vector points toward right
;            negative if wind vector points toward left
;   Along  – positive if wind vector points toward top
;            negative if wind vector points toward bottom
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1
RadToDeg = !KON.Misc.DoubleRadeg


PRINT_VALUES = 0    ;Set this to 0 for production
CR_AL = (CrOrAl EQ 1) ? 'Cross' : 'Along'

;------------------------------------------------------------------------
; Only operate on cameras with valid disparities, but exclude the
; reference camera.
;------------------------------------------------------------------------

Disparity[!KON.Instr.AN] = !KON.Misc.BADVALUE_REAL

ndxgood = WHERE(Disparity   NE !KON.Misc.BADVALUE_REAL AND $
                TerrainElev NE !KON.Misc.BADVALUE_REAL AND $
                Zenith      NE !KON.Misc.BADVALUE_REAL, numgood)

IF (numgood EQ 0) THEN RETURN

GoodCamNdx = ndxgood

;------------------------------------------------------------------------
; Initialize arrays. Array "FeatureHeight" contains the different model
; values of height above ellipsoid for which we want to solve for wind
; speed. These data pairs will allow us to calculate the analytic, linear
; regression equation that defines the height-wind solution line. Solve
; for winds at all valid cameras at each height using IDL's array-
; processing feature. See DNelson's 2013 paper for geometric details:
; http://www.mdpi.com/2072-4292/5/9/4593
;------------------------------------------------------------------------

RefTerrainElev = TerrainElev[!KON.Instr.AN] * 1000.0D
RefZenith = Zenith[!KON.Instr.AN] / RadToDeg

terrain_elev = TerrainElev[ndxgood] * 1000.0D
zenith_angle = Zenith[ndxgood] / RadToDeg
dispar_val   = Disparity[ndxgood] * !KON.Instr.HI_RES_PIX_SIZE * 1000.0D

;------------------------------------------------------------------------
; Precompute values that need not be in the loop.
;------------------------------------------------------------------------

cam_delta_seconds = !KON.Instr.MeanCamTimeFromAn[ndxgood]

radius_at_mean_terrain = RadiusLocal + (terrain_elev + RefTerrainElev) / 2.0

ref_alpha = !DPI - RefZenith
ref_alpha_term = SIN(ref_alpha) * (RadiusLocal + RefTerrainElev)

;------------------------------------------------------------------------
; Loop over the model heights and solve for wind speed at each.
;------------------------------------------------------------------------

FOR ihght=0,NumHts-1 DO BEGIN
   
   ;----------------------------------------------------------------------
   ; Compute the angle (relative to earth center) between the feature at
   ; reference camera time and a point directly above the terrain
   ; location of the reference camera's disparity. Skip this height if
   ; the feature elevation is lower than the terrain elevation.
   ;---------------------------------------------------------------------

   radius_at_feature = RadiusLocal + FeatureHeight[ihght]
   ref_mu = ASIN(ref_alpha_term / radius_at_feature)  
   ref_phi = RefZenith - ref_mu
   
   ;----------------------------------------------------------------------
   ; Compute the angles (relative to earth center) between the feature at
   ; comparison camera times and points directly above the terrain
   ; locations of the comparison cameras' disparities.
   ;---------------------------------------------------------------------
   
   cmp_alpha = !DPI - zenith_angle
   
   cmp_alpha_term = SIN(cmp_alpha) * (RadiusLocal + terrain_elev)
                     
   cmp_mu = ASIN(cmp_alpha_term / radius_at_feature)
   cmp_phi = zenith_angle - cmp_mu

   ;----------------------------------------------------------------------
   ; Compute the angles (relative to earth center) between the camera
   ; pairs' feature locations.
   ;----------------------------------------------------------------------

   disparity_angle = dispar_val / radius_at_mean_terrain
   solution_angle  = disparity_angle + ref_phi - cmp_phi

   ;----------------------------------------------------------------------
   ; Compute the total distances traveled and the windspeeds.
   ;----------------------------------------------------------------------

   HorzOffset[ihght,ndxgood] = solution_angle * radius_at_feature
   WindSpeed [ihght,ndxgood] = HorzOffset[ihght,ndxgood] / cam_delta_seconds
   
   ;----------------------------------------------------------------------
   ; Print debug information if requested.
   ;----------------------------------------------------------------------

   IF (PRINT_VALUES) THEN BEGIN
      PRINT, ''
      PRINT, FORMAT='(2A,I5,4F12.3)', $
             'CrOrAl, PtNum, Ht, RefZ, ref_alpha, ref_mu  ', $
             CR_AL, PtNum, FeatureHeight[ihght], RefZenith * RadToDeg, $
             ref_alpha * RadToDeg, ref_mu * RadToDeg
      PRINT, FORMAT='(A,9I12)',   'GudNdx', ndxgood
      PRINT, FORMAT='(A,9F12.0)', 'Radius', RadiusLocal
      PRINT, FORMAT='(A,9F12.3)', 'TerrHt', TerrainElev[ndxgood]
      PRINT, FORMAT='(A,9F12.3)', 'Zenith', zenith_angle * RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'Dispar', Disparity[ndxgood]
      PRINT, FORMAT='(A,9F12.3)', 'alpha ', cmp_alpha[ndxgood] * RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'mu    ', cmp_mu * RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'phi   ', cmp_phi* RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'beta  ', disparity_angle * RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'omaga ', solution_angle * RadToDeg
      PRINT, FORMAT='(A,9F12.3)', 'Dist  ', HorzOffset[ihght,ndxgood]
      PRINT, FORMAT='(A,9F12.3)', 'WndSpd', WindSpeed [ihght,ndxgood]
   ENDIF
ENDFOR

IF (PRINT_VALUES) THEN PRINT, ''

;------------------------------------------------------------------------
; Clean up.
;------------------------------------------------------------------------

ndxgood = 0
ndxneg = 0
cam_delta_seconds = 0
radius_at_feature = 0
radius_at_mean_terrain = 0
cmp_alpha_term = 0
cmp_alpha = 0
cmp_mu = 0
cmp_phi = 0
mean_terr_rad = 0
disparity_angle = 0
solution_angle = 0
terrain_elev = 0
zenith_angle = 0
dispar_val = 0
Status = 0

END  ;  GetHeightWindSolutionLine

;************************************************************************
PRO GetSolutionLineEquation, Heights, Winds, GoodCamNdx, Coeffs, Status
;************************************************************************
; For each camera at this point, find the equation of the best-fitting
; line that describes the points on the solution line for each camera.
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; Initialize values.
;------------------------------------------------------------------------

Status = -1

bad_cam = [-1]

numval1 = N_ELEMENTS(Heights)
numval2 = N_ELEMENTS(Winds[*,0])

IF (numval1 NE numval2) THEN RETURN

ndxs = WHERE(Winds NE !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs EQ 0) THEN RETURN

;------------------------------------------------------------------------
; Loop over the cameras.
;------------------------------------------------------------------------

isOK = 0
cnst = 0.0
num_cams = N_ELEMENTS(GoodCamNdx)

FOR icam=0,num_cams-1 DO BEGIN

   cam_num = GoodCamNdx[icam]

   ;---------------------------------------------------------------------
   ; Find the linear equation that describes the variation of wind with
   ; height for each camera. Across-track variation is perfectly linear.
   ;---------------------------------------------------------------------

   meas_err = SQRT(ABS(Winds[*,cam_num]))

   bcoeff = REGRESS(Heights[*], Winds[*,cam_num], CONST=acoeff, $
                    MEASURE_ERRORS=meas_err, CORRELATION=corr, $
                    STATUS=isOK)

   IF (ABS(corr) LT 0.99) THEN isOK = -1

   ;---------------------------------------------------------------------
   ; If the fit succeeded, save the coefficients. Otherwise remember the
   ; bad cameras.
   ;---------------------------------------------------------------------

   meas_err = 0

   IF (isOK EQ 0) THEN BEGIN
      Coeffs[0, cam_num] = acoeff
      Coeffs[1, cam_num] = bcoeff
;   print, format='(2I3,2F14.9,F11.5)', isOK, cam_num, acoeff, bcoeff, corr
   ENDIF ELSE BEGIN
      Coeffs[*, cam_num] = !KON.Misc.BADVALUE_REAL
      bad_cam = [bad_cam, cam_num]
   ENDELSE

ENDFOR
      
;------------------------------------------------------------------------
; Remove any bad cameras from the "good" list.
;------------------------------------------------------------------------

new_good_ndx = GoodCamNdx * 0

num_good = 0
num_bad  = N_ELEMENTS(bad_cam)
IF (num_bad GT 1) THEN BEGIN
   FOR icam=0,num_cams-1 DO BEGIN
      FOR ibad=1,num_bad-1 DO BEGIN
         IF (GoodCamNdx[icam] EQ bad_cam[ibad]) THEN GOTO, skip_cam
      ENDFOR
      new_good_ndx[num_good] = GoodCamNdx[icam]
      num_good += 1
skip_cam: 
   ENDFOR
   IF (num_good LT 1) THEN BEGIN
      GoodCamNdx = [-1]
      bad_cam = 0
      new_good_ndx = 0
      RETURN
   ENDIF
   GoodCamNdx = new_good_ndx[0:num_good-1]
   bad_cam = 0
   new_good_ndx = 0
ENDIF

Status = 0

END  ;  GetSolutionLineEquation

;************************************************************************
PRO GetSolutionLineIntercept, CoeffsCross, CoeffsAlong, Slope, $
                              GoodCamNdx, CorrHeightCams, $
                              WindCrossCams, WindAlongCams, Status
;************************************************************************
; For each camera at this point, determine the intersection of its
; height/wind-across/wind-along solution line with the direction line
; specified by the user as described by the Slope.
;------------------------------------------------------------------------

Status = -1

;------------------------------------------------------------------------
; Loop over the cameras.
;------------------------------------------------------------------------

num_cams = N_ELEMENTS(GoodCamNdx)

FOR icam=0,num_cams-1 DO BEGIN

   cam_num = GoodCamNdx[icam]

   ;---------------------------------------------------------------------
   ; Compute the height at the intersection with Slope. Then compute the
   ; winds at the intersection with Slope.
   ;---------------------------------------------------------------------

   numer = CoeffsCross[0,cam_num] * Slope - CoeffsAlong[0,cam_num]
   denom = CoeffsAlong[1,cam_num] - CoeffsCross[1,cam_num] * Slope
   corr_ht = numer / denom

   IF (corr_ht / 1000.0 GE $
         !KON.DataRgn.VALUE_MIN[!KON.DataRgn.TYPE_WINDCORR_HT] AND $
       corr_ht / 1000.0 LE $
         !KON.DataRgn.VALUE_MAX[!KON.DataRgn.TYPE_WINDCORR_HT]) THEN BEGIN
         
      CorrHeightCams[cam_num] = corr_ht

      WindCrossCams[cam_num] = CoeffsCross[0,cam_num] + $
                               CoeffsCross[1,cam_num] * $
                               CorrHeightCams[cam_num]

      WindAlongCams[cam_num] = CoeffsAlong[0,cam_num] + $
                               CoeffsAlong[1,cam_num] * $
                               CorrHeightCams[cam_num]
   ENDIF
ENDFOR

Status = 0

END  ;  GetSolutionLineIntercept

;************************************************************************
PRO SolveForHtsWinds, CoeffsCross, CoeffsAlong, Hght, WindC, WindA
;************************************************************************
; Use the linear height wind solution lines from all successful cameras
; to estimate height and winds without using the user's direction line.
; The point, in height / wind across / wind along space, where solution
; lines most nearly converge, might provide the solution.
; This is a research item - consider investigating later.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; 
;------------------------------------------------------------------------

iii=1


END  ;  SolveForHtsWinds

;************************************************************************
PRO ComputeHeightsAndWinds, RadiusLocal, NumPts, LatLonCoords, Slope, $
                            WindDirection, Offsets, CorrCoeffs, $
                            NoMorePlots, TerrainHts, CamZenCross, $
                            CamZenAlong, PtHtsCorr, PtWindCross, $
                            PtWindAlong, Retval
;************************************************************************
; Use the along and across disparities together with the camera geometry
; and terrain heights to compute the winds and wind-corrected heights.
; Compute heights at each point for all available camera pairs and find
; the best solution.Note that all offsets are considered positive if they
; are north or east, i.e. toward the right and top of the swath.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct             

;------------------------------------------------------------------------
; Set the minimum number of successful, valid camera pair retrievals
; required for a concensus height/wind result.
;------------------------------------------------------------------------

MinReqCams = 2

;------------------------------------------------------------------------
; If slopes exceeds a certain threshold, reset them to the max allowed.
;------------------------------------------------------------------------

MAX_SLOPE = !KON.Misc.SLOPE_MAX_THRESH2 ; ~= 86.2 deg

slopes = Slope
ndxs_pos = WHERE(slopes GE 0, num_pos, COMPLEMENT=ndxs_neg, NCOMPLEMENT=num_neg)
IF (num_pos GT 0) THEN slopes[ndxs_pos] = slopes[ndxs_pos] <   MAX_SLOPE
IF (num_neg GT 0) THEN slopes[ndxs_neg] = slopes[ndxs_neg] > (-MAX_SLOPE)

;------------------------------------------------------------------------
; Initialize values defining iteration to find solution lines. The lines
; are perfectly linear, so only enough points are needed to ensure that
; the correlation coefficient is 1.0.
;------------------------------------------------------------------------

begin_height = 0.0
numbr_height = 5
delta_height = 5000.0

heights = FINDGEN(numbr_height) * delta_height + begin_height

;------------------------------------------------------------------------
; Initialize arrays that will contain the different values of offset and
; windspeed for across and along directions. Each across/along pair
; corresponds to the same height above the ellipsoid.
;------------------------------------------------------------------------

HorzOffCross = FLTARR(numbr_height, !KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL
WindSpdCross = FLTARR(numbr_height, !KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL

HorzOffAlong = FLTARR(numbr_height, !KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL
WindSpdAlong = FLTARR(numbr_height, !KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Create arrays to contain the linear regression coefficients for cameras.
;------------------------------------------------------------------------

CoeffsCross = FLTARR(2,!KON.Instr.NCAM)
CoeffsAlong = FLTARR(2,!KON.Instr.NCAM)

;------------------------------------------------------------------------
; Allocate and initialize arrays returning per-camera height and wind
; solutions.
;------------------------------------------------------------------------

CorrHeightCams = FLTARR(!KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL
WindCrossCams  = FLTARR(!KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL
WindAlongCams  = FLTARR(!KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Initialize the output arrays for this routine.
;------------------------------------------------------------------------

PtHtsCorr[*]   = !KON.Misc.BADVALUE_REAL
PtWindCross[*] = !KON.Misc.BADVALUE_REAL
PtWindAlong[*] = !KON.Misc.BADVALUE_REAL

;------------------------------------------------------------------------
; Loop over the points comprising the plume direction line or the points
; in a region. For each point, compute the height/wind/wind solution line
; for each camera. Then solve for the best height/wind/wind triplet,
; given the user's input wind direction.
;------------------------------------------------------------------------

FOR ipts=0,NumPts-1 DO BEGIN

   ;---------------------------------------------------------------------
   ; Reinitialize the output arrays.
   ;---------------------------------------------------------------------

   GoodCamNdxCross = 0
   GoodCamNdxAlong = 0
   HorzOffCross[*,*] = !KON.Misc.BADVALUE_REAL
   WindSpdCross[*,*] = !KON.Misc.BADVALUE_REAL
   HorzOffAlong[*,*] = !KON.Misc.BADVALUE_REAL
   WindSpdAlong[*,*] = !KON.Misc.BADVALUE_REAL
   CoeffsCross[*,*]  = !KON.Misc.BADVALUE_REAL
   CoeffsAlong[*,*]  = !KON.Misc.BADVALUE_REAL
   CorrHeightCams[*] = !KON.Misc.BADVALUE_REAL
   WindCrossCams[*]  = !KON.Misc.BADVALUE_REAL
   WindAlongCams[*]  = !KON.Misc.BADVALUE_REAL
           
   ;---------------------------------------------------------------------
   ; Compute the across-track wind speeds for each camera pair at a range
   ; of heights.
   ;---------------------------------------------------------------------

   GetHeightWindSolutionLine, 1, ipts, $
                              DOUBLE(RadiusLocal), $
                              DOUBLE(REFORM(TerrainHts[*,ipts])), $
                              DOUBLE(REFORM(CamZenCross[*,ipts])), $
                              DOUBLE(REFORM(Offsets[0,*,ipts])), $
                              numbr_height, heights, WindSpdCross, $
                              HorzOffCross, GoodCamNdxCross, status
                                                      
   ;---------------------------------------------------------------------
   ; Get equation of best-fitting across-track line for each camera.
   ;---------------------------------------------------------------------

   GetSolutionLineEquation, heights, WindSpdCross, GoodCamNdxCross, $
                            CoeffsCross, status

   ;---------------------------------------------------------------------
   ; Compute the along-track wind speeds for each camera pair at a range
   ; of heights.
   ;---------------------------------------------------------------------

   GetHeightWindSolutionLine, 2, ipts, $
                              DOUBLE(RadiusLocal), $
                              DOUBLE(REFORM(TerrainHts[*,ipts])), $
                              DOUBLE(REFORM(CamZenAlong[*,ipts])), $
                              DOUBLE(REFORM(Offsets[1,*,ipts])), $
                              numbr_height, heights, WindSpdAlong, $
                              HorzOffAlong, GoodCamNdxAlong, status
                              
   ;---------------------------------------------------------------------
   ; Get equation of best-fitting along-track line for each camera.
   ;---------------------------------------------------------------------

   GetSolutionLineEquation, heights, WindSpdAlong, GoodCamNdxAlong, $
                            CoeffsAlong, status

   ;---------------------------------------------------------------------
   ; Find optimum height/windc/winda without using wind direction line
   ; from user! This is a research item - consider investigating later.
   ;---------------------------------------------------------------------

;   SolveForHtsWinds, CoeffsCross, CoeffsAlong, Hght, WindC, WindA
                            
   ;---------------------------------------------------------------------
   ; Only use cameras for which both across and along results are valid.
   ;---------------------------------------------------------------------

   IF (~ ARRAY_EQUAL(GoodCamNdxCross, GoodCamNdxAlong)) THEN CONTINUE

   GoodCamNdx = GoodCamNdxAlong

   ;---------------------------------------------------------------------
   ; Apply the user's input wind direction to each camera's solution to
   ; find the height/wind/wind triplet for each camera.
   ;---------------------------------------------------------------------

   GetSolutionLineIntercept, CoeffsCross, CoeffsAlong, slopes[ipts], $
                             GoodCamNdx, CorrHeightCams, WindCrossCams, $
                             WindAlongCams, rtrn
                           
   ;---------------------------------------------------------------------
   ; Determine the best concensus height/wind/wind solution for all
   ; cameras at this point using the current slope.
   ;---------------------------------------------------------------------
                      
   GetBestCameraHtWndFit, ipts, GoodCamNdx, slopes[ipts], $
                          WindDirection[ipts], CorrHeightCams, $
                          WindCrossCams, WindAlongCams, $
                          REFORM(TerrainHts[*,ipts]), $
                          REFORM(Offsets[*,*,ipts]), $
                          REFORM(CorrCoeffs[*,ipts]), NoMorePlots, $
                          GoodCamsOut, corr_height, wind_cross, $
                          wind_along, rtrn

   ;---------------------------------------------------------------------
   ; Save the height/wind/wind/fit values if they're within the range
   ; the user specified.
   ;---------------------------------------------------------------------

   IF (rtrn GE 0 AND N_ELEMENTS(GoodCamsOut) GE MinReqCams) THEN BEGIN
      PtHtsCorr[ipts] = corr_height * 0.001
      PtWindCross[ipts] = wind_cross
      PtWindAlong[ipts] = wind_along
   ENDIF

ENDFOR

;------------------------------------------------------------------------
; Final cleanup.
;------------------------------------------------------------------------

HorzOffCross = 0
WindSpdCross = 0
HorzOffAlong = 0
WindSpdAlong = 0
CoeffsCross  = 0
CoeffsAlong  = 0
angle_values = 0
CorrHeightCams = 0
WindCrossCams  = 0
WindAlongCams  = 0

END  ;  ComputeHeightsAndWinds

;***************************************************************************
PRO DisplayHtsAndCCoeffs, Type, CamData, GoodCams, RangeMatrix, $
                          RangeWeight, BestValue, NoMorePlots
;***************************************************************************
; If testing correlation and clustering of heights and winds, display here.
; Use per-camera data.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE
            
IF (NoMorePlots) THEN RETURN

;---------------------------------------------------------------------------
; Print critical values to standard output.
;---------------------------------------------------------------------------

value_std_dev = STDDEV(CamData.cam_value)

PRINT, RangeMatrix
PRINT, ''
PRINT, RangeWeight
PRINT, ''
PRINT, 'input data values =     ', CamData.cam_value
PRINT, 'all sorted cam ndxs =   ', CamData.cam_number
PRINT, 'std dev of cam values = ', value_std_dev
PRINT, 'best cam indexes =      ', GoodCams
PRINT, 'best weighted value =   ', BestValue

;---------------------------------------------------------------------------
; Initialize values. First define the numeric digits (0-9) as vector symbols
; to use in plotting camera locations.
;---------------------------------------------------------------------------

cam_sym_x = $
  [[-3.0, 3.0, 3.0,-3.0,-3.0,-3.0,-3.0], [-2.0, 0.0, 0.0,-2.0, 2.0, 2.0, 2.0], $
   [-3.0, 3.0, 3.0,-3.0,-3.0, 3.0, 3.0], [-3.0, 3.0, 3.0,-1.0, 3.0, 3.0,-3.0], $
   [-3.0,-3.0, 3.0, 3.0, 3.0, 3.0, 3.0], [ 3.0,-3.0,-3.0, 3.0, 3.0, 1.0,-3.0], $
   [ 3.0,-3.0,-3.0, 3.0, 3.0,-3.0,-3.0], [-3.0, 3.0, 3.0,-3.0,-3.0,-3.0,-3.0], $
   [-3.0, 3.0, 3.0,-3.0,-3.0,-3.0, 3.0], [ 3.0, 3.0,-3.0,-3.0, 3.0, 3.0, 3.0]] / 6.0
cam_sym_y = $
  [[ 6.0, 6.0,-6.0,-6.0, 6.0, 6.0, 6.0], [ 4.0, 6.0,-6.0,-6.0,-6.0,-6.0,-6.0], $
   [ 6.0, 6.0, 2.0,-4.0,-6.0,-6.0,-6.0], [ 6.0, 6.0, 0.0, 0.0, 0.0,-6.0,-6.0], $
   [ 6.0, 0.0, 0.0, 6.0,-6.0,-6.0,-6.0], [ 6.0, 6.0, 0.0, 0.0,-4.0,-6.0,-6.0], $
   [ 6.0, 6.0,-6.0,-6.0, 0.0, 0.0, 0.0], [ 6.0, 6.0, 2.0,-4.0,-6.0,-6.0,-6.0], $
   [ 6.0, 6.0,-6.0,-6.0, 6.0, 0.0, 0.0], [-6.0, 6.0, 6.0, 0.0, 0.0, 0.0, 0.0]] / 6.0

xsize = 900 & ysize = 600 & xpos  =  50 & ypos  = 100
       
IF (Type EQ 1) THEN BEGIN
   tname = 'Zero-Wind Height'
   xname = 'Height ASL (km)'
   min_val = -1.0
   max_val =  6.0
   values = CamData.cam_value / 1000.0
   best_value = BestValue / 1000.0
ENDIF
IF (Type EQ 2) THEN BEGIN
   tname = 'Wind-Corrected Height'
   xname = 'Height ASL (km)'
   min_val = -1.0
   max_val =  6.0
   values = CamData.cam_value / 1000.0
   best_value = BestValue / 1000.0
ENDIF
IF (Type EQ 3) THEN BEGIN
   tname = 'Across-Track Wind'
   xname = 'Wind Speed (m/s)'
   min_val = -10.0
   max_val =  10.0
   values = CamData.cam_value
   best_value = BestValue
ENDIF
IF (Type EQ 4) THEN BEGIN
   tname = 'Along-Track Wind'
   xname = 'Wind Speed (m/s)'
   min_val = -10.0
   max_val =  10.0
   values = CamData.cam_value
   best_value = BestValue
ENDIF

yname = 'Correlation Coefficient'

title = 'Clustering of ' + tname + 's by Camera w/ Correlation Coeffs'
subtitle = 'Numbers are cameras (0=Df => 8=Da); ' + $
           'Red cams = included, Black cams = excluded; ' + $
           'Green line = best fit height'

;---------------------------------------------------------------------------
; Find the min and max of values for this point.
;---------------------------------------------------------------------------

num_val = N_ELEMENTS(values)
IF (MIN(values) LT min_val) THEN min_val = FLOOR(MIN(values))
IF (MAX(values) GT max_val) THEN max_val = CEIL(MAX(values))
min_corr = (MIN(CamData.corr_coeff) LT 0.0) ? -1.0 : 0.0
max_corr =  1.1

vert_txt_sep = (max_val - min_val) * 0.05

;---------------------------------------------------------------------------
; Construct the plot framework.
;---------------------------------------------------------------------------

WINDOW, TITLE=title, /FREE, XSIZE=xsize, YSIZE=ysize, XPOS=xpos, YPOS=ypos
wndw = !D.WINDOW

PLOT, TITLE=title, SUBTITLE=subtitle, XTITLE=xname, YTITLE=yname, COLOR=0, $
      LINESTYLE=0, [0.0,0.01], [0.0,0.0], BACKGROUND=16777215, $
      XTICKLEN=1, YTICKLEN=1, XSTYLE=1, YSTYLE=1, XGRIDSTYLE=1, YGRIDSTYLE=1, $
      XMARGIN=[10,3], YMARGIN=[5,3], XRANGE=[min_val,max_val], $
      YRANGE=[min_corr,max_corr]

;---------------------------------------------------------------------------
; Loop over the cameras.
;---------------------------------------------------------------------------

FOR icam=0,num_val-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Draw each camera's hght/wind-across and hght/wind-along points. Use
   ; colors to identify cameras and symbols to identify cross/along winds.
   ; Also plot the final best-fit values.
   ;------------------------------------------------------------------------
   
   val = values[icam]
   corr = CamData[icam].corr_coeff
   
   USERSYM, cam_sym_x[*,CamData[icam].cam_number], $
            cam_sym_y[*,CamData[icam].cam_number]
   ndxs = WHERE(CamData[icam].cam_number EQ GoodCams, numndxs)
   OPLOT, [val], [corr], THICK=2, PSYM=8, SYMSIZE=2, $
          COLOR=(numndxs GT 0) ? '0000FF'XUL : '000000'XUL
ENDFOR

;------------------------------------------------------------------------
; Draw a vertical line at the computed best estimate.
;------------------------------------------------------------------------

OPLOT, [best_value,best_value], [min_corr,max_corr], THICK=1, COLOR='00FF00'XUL

;------------------------------------------------------------------------
; Let user study plot before deleting.
;------------------------------------------------------------------------

rtrn = DIALOG_MESSAGE('Skip plots?', /QUESTION)
IF (STRUPCASE(rtrn) EQ 'YES') THEN NoMorePlots = 1

SafeWDELETE, wndw, DidIt

val_ndxs = 0
wc_ndxs = 0
wa_ndxs = 0
values = 0
ndxs = 0
   
END  ;  DisplayHtsAndCCoeffs

