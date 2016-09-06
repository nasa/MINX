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
FUNCTION GetBiomeClassAtLatLong, Lon, Lat
;***************************************************************************
; Return the type code of the biome at the specified lon/lat coordinates.
;---------------------------------------------------------------------------

COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

NUM_SHELLS = 5   ; The number of squares or shells of increasing size around
; retrieved bad class location to search for a replacement.
; Each shell's pixels are ~5 km wide!

;---------------------------------------------------------------------------
; Convert lat/lon to array indices. map origin is at lon = -180 and lat = +90
;---------------------------------------------------------------------------

num_lon = (SIZE(Biome_grid))[1]
num_lat = (SIZE(Biome_grid))[2]

lon_coeff = (360.0 - Biome_grid_spacing) / (360.0 * Biome_grid_spacing)
lat_coeff = (180.0 - Biome_grid_spacing) / (180.0 * Biome_grid_spacing) * (-1.0)

lon_ndx = FLOOR(Lon * lon_coeff + (num_lon - 1.0) / 2.0)
lat_ndx = FLOOR(Lat * lat_coeff + (num_lat - 1.0) / 2.0)

;---------------------------------------------------------------------------
; Get the biome type code at the specified index pair.
;---------------------------------------------------------------------------

type_code = Biome_grid[lon_ndx, lat_ndx]

;---------------------------------------------------------------------------
; Refine the biome type code by changing every occurrence of the rejected
; classes - water (0) and snow (15) - to the class of the nearest region
; that is not one of these.
;---------------------------------------------------------------------------

done_flag = 0
best_distance = 99999.0
best_type_code = type_code

IF (type_code EQ 0 OR type_code EQ 15) THEN BEGIN  ; water or snow/ice

   ;------------------------------------------------------------------------
   ; Loop over square groups of biome pixels (shells) surrounding the point
   ; in order of increasing distance NUM_SHELLS times. Stop as soon as a
   ; shell has been completed and a biome pixel is found in the shell that
   ; is not one of the rejected biomes. Use the biome class that is closest
   ; to the center point to replace the center point's rejected biome class.
   ;------------------------------------------------------------------------
   
   n_pix = 8 * NUM_SHELLS + 4
   xlon = INTARR(n_pix)
   ylat = INTARR(n_pix)
   
   FOR ishell=1,NUM_SHELLS DO BEGIN
   
      num_pt = 0
      FOR xpnt=-ishell,ishell DO BEGIN
         FOR ypnt=-ishell,ishell,ishell*2 DO BEGIN
            IF (lon_ndx + xpnt LT 0 OR lon_ndx + xpnt GT 7199 OR $
               lat_ndx + ypnt LT 0 OR lat_ndx + ypnt GT 3599) THEN CONTINUE
            xlon[num_pt] = lon_ndx + xpnt
            ylat[num_pt] = lat_ndx + ypnt
            num_pt += 1
         ENDFOR
      ENDFOR
      FOR ypnt=-ishell,ishell DO BEGIN
         FOR xpnt=-ishell,ishell,ishell*2 DO BEGIN
            IF (lon_ndx + xpnt LT 0 OR lon_ndx + xpnt GT 7199 OR $
               lat_ndx + ypnt LT 0 OR lat_ndx + ypnt GT 3599) THEN CONTINUE
            xlon[num_pt] = lon_ndx + xpnt
            ylat[num_pt] = lat_ndx + ypnt
            num_pt += 1
         ENDFOR
      ENDFOR
      
      FOR ipnt=0,num_pt-1 DO BEGIN
      
         new_type_code = Biome_grid[xlon[ipnt], ylat[ipnt]]
         
         IF (new_type_code NE 0 AND new_type_code NE 15) THEN BEGIN ; water or snow
         
            ;---------------------------------------------------------------
            ; If this biome pixel not one of the rejected classes, find its
            ; distance from the center point and process further. We need to
            ; convert lat/lon indices at biome pixel centers to meters first.
            ;---------------------------------------------------------------
            
            new_dist = MAP_2POINTS((xlon[ipnt] * Biome_grid_spacing - 180.0D), $
               (ylat[ipnt] * Biome_grid_spacing -  90.0D), $
               (lon_ndx * Biome_grid_spacing - 180.0D), $
               (lat_ndx * Biome_grid_spacing -  90.0D), $
               /METERS)
               
            ;---------------------------------------------------------------
            ; If this biome pixel is the closest so far, then save it.
            ;---------------------------------------------------------------
            
            IF (new_dist LT best_distance) THEN BEGIN
               best_type_code = new_type_code
               best_distance = new_dist
               done_flag = 1
            ENDIF
         ENDIF
         
      ENDFOR
      
      ;---------------------------------------------------------------------
      ; If a biome class replacement was found in this shell, we're done.
      ;---------------------------------------------------------------------
      
      IF (done_flag EQ 1) THEN BREAK
      
   ENDFOR
   
   xlon = 0
   ylat = 0
   
ENDIF

RETURN, best_type_code
   
END  ;  GetBiomeClassAtLatLong

;***************************************************************************
FUNCTION GetBiomeClassName, BiomeTypeCode
;***************************************************************************
; Given a biome type code, return the biome name. If a code greater than
; the largest code value is passed, return all the names.
;---------------------------------------------------------------------------

COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

invalid_value = N_ELEMENTS(Biome_names)

IF (BiomeTypeCode GE invalid_value) THEN BEGIN
   RETURN, Biome_names
ENDIF ELSE BEGIN
   IF (BiomeTypeCode LT 0) THEN BEGIN
      RETURN, Biome_names[invalid_value-1]
   ENDIF ELSE BEGIN
      RETURN, Biome_names[BiomeTypeCode]
   ENDELSE
ENDELSE
   
END  ;  GetBiomeClassName

;***************************************************************************
PRO ResetFirePixList, pRgnObj, pFireObj, PixSep, NumFound
;***************************************************************************
; If there were fire pixels in a region that is being deleted, then any
; fire pixels that were in the region must have their "used" flags reset so
; they can be used again. Also reset fire pixels after a plume has been
; digitized so they can be captured again if the plume is re-digitized (e.g.
; for retrieval with another band).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1
NumFound = 0

;---------------------------------------------------------------------------
; Loop over all the pixels in the region list.
;---------------------------------------------------------------------------

pNextRgnPix = (*pRgnObj).pNextLinePt

WHILE (PTR_VALID(pNextRgnPix)) DO BEGIN
   PtRgnData = *((*pNextRgnPix).pData)

   Block = PtRgnData.block
   Samp  = PtRgnData.cross275
   Line  = PtRgnData.along275
   Power = PtRgnData.modis_data[0]

   ;------------------------------------------------------------------------
   ; Loop over all the fire pixels to see if one matches.
   ;------------------------------------------------------------------------

   pNextFirePix = (*pFireObj).pNextLinePt

   WHILE (PTR_VALID(pNextFirePix)) DO BEGIN
      PtFireData = *((*pNextFirePix).pData)

      ;---------------------------------------------------------------------
      ; Test if the fire pixels have associated MODIS data. This will be
      ; true if the data were read directly from MODIS MOD14 granules, but
      ; false if read from ModVolc database files. Test if the fire pixel
      ; is close enough to the region point. If so, copy the data over.
      ;---------------------------------------------------------------------

      IF (PtFireData.modis_data[1] NE 0.0) THEN BEGIN

         samp_diff = FLOAT(PtFireData.cross275 - Samp)
         line_diff = FLOAT(PtFireData.along275 - Line)

         dist = FIX(SQRT(samp_diff * samp_diff + line_diff * line_diff))
         fire_power = (*((*pNextFirePix).pData)).modis_data[0]

         IF ((PtFireData.block EQ Block) AND (dist LE PixSep) AND $
             (ABS(Power - fire_power) LT 1.0)) THEN BEGIN
            NumFound += 1
            (*((*pNextFirePix).pData)).modis_data[1] = 0.0
            BREAK
         ENDIF

      ENDIF

      pNextFirePix = (*pNextFirePix).pNextSib
   ENDWHILE

   pNextRgnPix = (*pNextRgnPix).pNextSib
ENDWHILE

Retval = 0

END  ;  ResetFirePixList

;***************************************************************************
PRO CopyFireDataToRegion, pFireObj, PixSep, Block, Samp, Line, ModisData, $
                          NumFound, Retval
;***************************************************************************
; For each point in the region, see if there is a fire pixel at that coord.
; If so, copy the fire pixel data to the region point.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Test if the fire pixels have associated MODIS data. This will be true if
; the data were read directly from MODIS MOD14 granules, but false if read
; from ModVolc database files. Test if the fire pixel is close enough to
; the region point. If so, copy the data over.
;---------------------------------------------------------------------------

pNextPt = (*pFireObj).pNextLinePt

WHILE (PTR_VALID(pNextPt)) DO BEGIN
   PtData = *((*pNextPt).pData)

   IF (PtData.modis_data[1] EQ 0.0) THEN BEGIN

      samp_diff = FLOAT(PtData.cross275 - Samp)
      line_diff = FLOAT(PtData.along275 - Line)

      dist = FIX(SQRT(samp_diff * samp_diff + line_diff * line_diff))

      IF ((PtData.block EQ Block) AND (dist LE PixSep)) THEN BEGIN
         ModisData = PtData.modis_data
         NumFound += 1
         (*((*pNextPt).pData)).modis_data[1] = 1.0
         BREAK
      ENDIF

   ENDIF

   pNextPt = (*pNextPt).pNextSib
ENDWHILE

Retval = 0

END  ;  CopyFireDataToRegion

;***************************************************************************
FUNCTION ComputeLineLength, NumPts, SomCoords
;***************************************************************************
; Compute the length of the polyline in km. If the polyline is a
; closed polygon, this assumes that the first and last points on the
; polygon's polyline are identical.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

cum_dist = 0.0

som_km = SomCoords

;---------------------------------------------------------------------------
; Loop over all the points starting with the 2nd one.
;---------------------------------------------------------------------------

FOR ipts=1,NumPts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Get the distance from the previous point to this one.
   ;------------------------------------------------------------------------

   xdist = som_km[0,ipts] - som_km[0,ipts-1]
   ydist = som_km[1,ipts] - som_km[1,ipts-1]

   dist = SQRT(xdist * xdist + ydist * ydist)

   ;------------------------------------------------------------------------
   ; Accumulate the distance.
   ;------------------------------------------------------------------------

   cum_dist += dist

ENDFOR

;---------------------------------------------------------------------------
; Return the cumulative distance.
;---------------------------------------------------------------------------

RETURN, cum_dist

END  ;  ComputeLineLength

;***************************************************************************
PRO GetSwathDirRelGeoNorth, State, num_pts_line, CamOffsetCrds, AngleToSomN, $
                            AngleToSwathN, Retval
;***************************************************************************
; Find the angle clockwise from geographic north to 1) SOM north (MISR data
; columns) and to 2) swath ground-track direction (diagonal edge of data).
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Create output arrays.
;---------------------------------------------------------------------------

AngleToSomN   = DBLARR(!KON.Instr.NCAM, num_pts_line) + !KON.Misc.BADVALUE_REAL
AngleToSwathN = DBLARR(!KON.Instr.NCAM, num_pts_line) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Loop over the cameras.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Extract the point coordinates for this camera. Just skip if there are
   ; no good values.
   ;------------------------------------------------------------------------

   coords_cross = REFORM(CamOffsetCrds[0,icam,*])
   coords_along = REFORM(CamOffsetCrds[1,icam,*])
   ndxgood = WHERE(coords_cross NE !KON.Misc.BADVALUE_REAL AND $
                   coords_along NE !KON.Misc.BADVALUE_REAL, numgood)
   IF (numgood EQ 0) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Get the longitude/latitude coordinates for the original points. First
   ; back out the reversed direction of along coordinates (-1).
   ;------------------------------------------------------------------------

   wndw_max = CoordStruct.(0).NumBlk * CoordStruct.(0).DimAlong - 1
   coords_along[ndxgood] = wndw_max - coords_along[ndxgood] - 1
   
   coords_copy = [[coords_cross[ndxgood]], [coords_along[ndxgood]]]
   coords_copy = TRANSPOSE(coords_copy)

   WndwCrdToMisrCrd,  State.curframe, coords_copy, misr_coord, Retval
   MisrCrdToSomCrd,   State.curframe, misr_coord,  som_coord1, Retval
   SomCrdToLonlatCrd, State.curframe, som_coord1, lonlat_coord1, Retval

   coords_cross = 0
   coords_along = 0
   coords_copy = 0
   misr_coord = 0

   ;------------------------------------------------------------------------
   ; Get the longitude/latitude coordinates for new comparison points.
   ; Create a complimentary point about 10 km due geographic north of each
   ; (along the same meridian).
   ;------------------------------------------------------------------------

   lonlat_coord2 = lonlat_coord1
   lonlat_coord2[1,*] += 0.05D  ;  ~ 5 km

   ;------------------------------------------------------------------------
   ; Find the angle clockwise from geographic north to "up" along SOM north
   ; using pixel coordinates. Angles between geographic north and SOM north
   ; must be the same for all cameras at each point.
   ;------------------------------------------------------------------------

   LonlatCrdToSomCrd, 1, 0, lonlat_coord2, som_coord2, retval
   
   north_dx = DOUBLE(som_coord2[0,*]) - som_coord1[0,*]
   north_dy = DOUBLE(som_coord1[1,*]) - som_coord2[1,*]

   som_coord1 = 0
   som_coord2 = 0
   lonlat_coord1 = 0
   lonlat_coord2 = 0

   trueN_from_somN = DBLARR(numgood) + !KON.Misc.BADVALUE_REAL

   ndxs = WHERE(ABS(north_dy) GE 0.0001, numndxs)
   IF (numndxs GT 0) THEN $
      trueN_from_somN[ndxs] = ATAN(north_dx, north_dy) * !RADEG
   
   ndxs = WHERE(ABS(north_dy) LT 0.0001 AND north_dx GE 0.0, numndxs)
   IF (numndxs GT 0) THEN trueN_from_somN[ndxs] = 90.0
   
   ndxs = WHERE(ABS(north_dy) LT 0.0001 AND north_dx LT 0.0, numndxs)
   IF (numndxs GT 0) THEN trueN_from_somN[ndxs] = -90.0

   ndxs = WHERE(trueN_from_somN LT 0.0, numndxs)
   IF (numndxs GT 0) THEN trueN_from_somN[ndxs] += 360.0

   trueN_from_somN = 360.0 - trueN_from_somN

   ;------------------------------------------------------------------------
   ; Copy the north to SOM angle results to the output preserving bad values.
   ;------------------------------------------------------------------------

   temp_ary = DBLARR(num_pts_line) + !KON.Misc.BADVALUE_REAL
   temp_ary[ndxgood] = trueN_from_somN
   AngleToSomN[icam,*] = temp_ary
   
   north_dx = 0
   north_dy = 0
   trueN_from_somN = 0
   
   ;------------------------------------------------------------------------
   ; Find angle clockwise from geographic north to "up" along swath ground
   ; track. Angles between geographic north and SOM north are the same for
   ; all cameras at each point. Preserve bad values.
   ;------------------------------------------------------------------------

   temp_ary[ndxgood] += SomToSwathAngles[icam] * !RADEG
   AngleToSwathN[icam,*] = temp_ary

   temp_ary = 0
   ndxgood = 0

ENDFOR

;---------------------------------------------------------------------------
; Store the An camera's angle values in all cameras for each point. This is
; because the rotation of the direction line and disparities is also done
; for a constant (An) angle. Too bad.
;---------------------------------------------------------------------------

FOR ipts=0,num_pts_line-1 DO BEGIN
   temp_ary = REFORM(AngleToSomN[*,ipts])
   ndxgood = WHERE(temp_ary NE !KON.Misc.BADVALUE_REAL, numgood)
   IF (numgood GT 0) THEN BEGIN
      temp_ary[ndxgood] = AngleToSomN[!KON.Instr.AN,ipts]
      AngleToSomN[*,ipts] = temp_ary
   ENDIF
   temp_ary = REFORM(AngleToSwathN[*,ipts])
   ndxgood = WHERE(temp_ary NE !KON.Misc.BADVALUE_REAL, numgood)
   IF (numgood GT 0) THEN BEGIN
      temp_ary[ndxgood] = AngleToSwathN[!KON.Instr.AN,ipts]
      AngleToSwathN[*,ipts] = temp_ary
   ENDIF

   temp_ary = 0
   ndxgood = 0
ENDFOR

Retval = 0

END  ;  GetSwathDirRelGeoNorth

;***************************************************************************
PRO GetSOMandTRUEnorth, CrdsPt1, CrdsPt2, LonLat1, DirCWfromSomN, $
                        DirCWfromTruN
;***************************************************************************
; Given pairs of points with SOM coordinates, find the angles between the
; line formed by them with 1) SOM north and 2) geographic north.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Find the direction from point 1 to point 2 measured clockwise from SOM
; north (0->360). Adjust the angle to account for SOM along coord inversion.
;---------------------------------------------------------------------------

plume_dx =   CrdsPt1[0] - CrdsPt2[0]
plume_dy = -(CrdsPt1[1] - CrdsPt2[1])

IF (ABS(plume_dy) GE 0.001) THEN BEGIN
   DirCWfromSomN = ATAN(plume_dx, plume_dy) * !RADEG
ENDIF ELSE BEGIN
   IF (plume_dx GT 0.0) THEN BEGIN
      DirCWfromSomN = 90.0
   ENDIF ELSE BEGIN
      DirCWfromSomN = -90.0
   ENDELSE
ENDELSE

IF (DirCWfromSomN LT 0.0) THEN DirCWfromSomN += 360.0

;---------------------------------------------------------------------------
; Find the direction of local north measured clockwise from SOM north
; (0->360). Adjust the angle to account for the inversion of the SOM along
; coordinate.
;---------------------------------------------------------------------------

lonlat2 = LonLat1
lonlat2[1] += 0.03   ; approximately 3 km away from 1st point

LonlatCrdToSomCrd, 1, 0, lonlat2, som2, retval
   
north_dx =   som2[0] - CrdsPt1[0]
north_dy = -(som2[1] - CrdsPt1[1])

IF (ABS(north_dy) GE 0.001) THEN BEGIN
   trueN_from_somN = ATAN(north_dx, north_dy) * !RADEG
ENDIF ELSE BEGIN
   IF (north_dx GT 0.0) THEN BEGIN
      trueN_from_somN = 90.0
   ENDIF ELSE BEGIN
      trueN_from_somN = -90.0
   ENDELSE
ENDELSE

IF (trueN_from_somN LT 0.0) THEN trueN_from_somN += 360.0

;---------------------------------------------------------------------------
; Find the direction of the plume point pair measured clockwise from local
; north (0->360). 
;---------------------------------------------------------------------------

DirCWfromTruN = DirCWfromSomN - trueN_from_somN
IF (DirCWfromTruN LT 0.0) THEN DirCWfromTruN += 360.0

END  ;  GetSOMandTRUEnorth

;***************************************************************************
PRO ResampleDirLine, State, pThisRgn, PointSep, retval
;***************************************************************************
; Replace the linked list of points representing a direction line in a plume
; region with a linked list of points that has been resampled to a regular
; interval over the trace of the dir line. This is called only by the plume
; center line option.
;---------------------------------------------------------------------------

COMMON data_structs, region_data, linept_data

COMPILE_OPT IDL2, LOGICAL_PREDICATE

retval = -1

;---------------------------------------------------------------------------
; Collect the SOM coordinates from the input linked list of points.
;---------------------------------------------------------------------------

pHeadPt = (*pThisRgn).pNextLinePt

LListGetCoordArray, State.sizey, pHeadPt, num_old_pts, som_coords, $
                    image_coords, misr_coords, lonlat_coords

;---------------------------------------------------------------------------
; Create a pointer to a new array containing the MISR coordinates of the
; original (unsplined) direction line points. Then save the pointer in the
; region object.
;---------------------------------------------------------------------------

(*((*pThisRgn).pData)).direc_pts_orig = PTR_NEW(misr_coords)

;---------------------------------------------------------------------------
; Construct a parametric spline to smoothly connect the digitized points on
; either the direction line (for plumes) or the data line (for line objects,
; where the direction line doubles as the points where heights are to be
; determined). Interpolate on the spline at the spatial interval in meters
; specified for Plume Point Grid spacing in the digitizing dialog box.
;---------------------------------------------------------------------------

SPLINE_P, DOUBLE(som_coords[0,*]), DOUBLE(som_coords[1,*]), Xout, Yout, $
          INTERVAL=PointSep
new_som_crds = TRANSPOSE([[Xout], [Yout]])
num_spline_pts = N_ELEMENTS(Xout)

;---------------------------------------------------------------------------
; There must be at least 3 valid points on the resampled line to continue.
;---------------------------------------------------------------------------

ndxs = WHERE(Yout GT 0.0, numndx)
IF (numndx LT 3) THEN RETURN

;---------------------------------------------------------------------------
; Construct a distance array containing the distance from the source point
; for each point on the line.
;---------------------------------------------------------------------------

new_dist = FLTARR(num_spline_pts)
cum_dist = 0.0

FOR ipts=1,num_spline_pts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Get the distance from the previous point to this one.
   ;------------------------------------------------------------------------

   xdist = new_som_crds[0,ipts] - new_som_crds[0,ipts-1]
   ydist = new_som_crds[1,ipts] - new_som_crds[1,ipts-1]
   dist  = SQRT(xdist * xdist + ydist * ydist)

   ;------------------------------------------------------------------------
   ; Accumulate the distance.
   ;------------------------------------------------------------------------

   cum_dist += dist
   new_dist[ipts] = cum_dist
ENDFOR

;---------------------------------------------------------------------------
; Convert the SOM coordinates to additional coordinates and save. Remove any
; coordinates that are out of bounds.
;---------------------------------------------------------------------------

whichorbit = (State.curFrame GT 9)

SomCrdToLonlatCrd, State.Curframe, FLOAT(new_som_crds), lonlat_crds, retval
SomCrdToMisrCrd,  State.Curframe, 0, FLOAT(new_som_crds), misr_crds, retval
MisrCrdToWndwCrd, State.Curframe, misr_crds, wndw_crds, 1, retval

ndxs = WHERE(wndw_crds[1,*] GT 0, numndxs, NCOMPLEMENT=numbad)
IF (numbad GT 0 AND numndxs GT 0) THEN BEGIN
   mssg = ['You have created a direction line which, when splined, will', $
           'extend outside the window. The line is rejected. Try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER)
   retval = -2
   GOTO, CleanItUp
ENDIF

;---------------------------------------------------------------------------
; Construct an array of point structures that will be copied to the linked
; list as it is created.
;---------------------------------------------------------------------------

dummy = 0
iret = InitLinePtData(dummy)

temp_pts = REPLICATE(linept_data, num_spline_pts)

;---------------------------------------------------------------------------
; Loop over all the points in the interpolated spline list.
;---------------------------------------------------------------------------

FOR ipts=0,num_spline_pts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Find the slope at the plume point measured with respect to the MISR
   ; grid lines. For points interior to a line, this is the average of the
   ; adjacent line segment slopes. For end points, it is the slope of the
   ; end segment. Reverse the sign to account for the SOM coords increasing
   ; to the south. Take care of the case when the line is vertical by
   ; setting any slope greater than a threshold to that threshold value.
   ;------------------------------------------------------------------------

   min_dx = 0.00001
   new_slope = !KON.Misc.SLOPE_MAX_THRESH1

   IF (ipts GT 0 AND ipts LT num_spline_pts-1) THEN BEGIN
      dy = -(new_som_crds[1,ipts+1] - DOUBLE(new_som_crds[1,ipts-1]))
      dx =  (new_som_crds[0,ipts+1] - DOUBLE(new_som_crds[0,ipts-1]))
      IF (ABS(dx) GE min_dx) THEN BEGIN
         new_slope = dy / dx
         sign = new_slope / ABS(new_slope)
         IF (ABS(new_slope) GT !KON.Misc.SLOPE_MAX_THRESH1) THEN $
            new_slope = !KON.Misc.SLOPE_MAX_THRESH1 * sign
      ENDIF
   ENDIF ELSE BEGIN
      IF (ipts EQ 0) THEN BEGIN
         dy = -(new_som_crds[1,ipts+1] - DOUBLE(new_som_crds[1,ipts]))
         dx =  (new_som_crds[0,ipts+1] - DOUBLE(new_som_crds[0,ipts]))
      ENDIF
      IF (ipts EQ num_spline_pts-1) THEN BEGIN
         dy = -(new_som_crds[1,ipts] - DOUBLE(new_som_crds[1,ipts-1]))
         dx =  (new_som_crds[0,ipts] - DOUBLE(new_som_crds[0,ipts-1]))
      ENDIF
      IF (ABS(dx) GT min_dx) THEN new_slope = FLOAT(dy / dx)
      IF (ABS(new_slope) GT !KON.Misc.SLOPE_MAX_THRESH1) THEN $
         new_slope = !KON.Misc.SLOPE_MAX_THRESH1 * new_slope / ABS(new_slope)
   ENDELSE

   ;------------------------------------------------------------------------
   ; Find the direction of the plume point pair measured clockwise from SOM
   ; north (0->360). Adjust the angle to account for the SOM along coord
   ; inversion.
   ;------------------------------------------------------------------------

   iipts = ipts
   IF (ipts EQ 0) THEN iipts = ipts + 1
   IF (ipts EQ num_spline_pts-1 AND ipts GT 1) THEN iipts = ipts - 1

   GetSOMandTRUEnorth, REFORM(new_som_crds[*,iipts]), $
                       REFORM(new_som_crds[*,iipts-1]), $
                       lonlat_crds[0:1,iipts], $
                       plume_dir_CW_from_som_N, $
                       plume_dir_CW_from_tru_N

   ;------------------------------------------------------------------------
   ; Store the point coordinates in the temporary structure.
   ;------------------------------------------------------------------------

   temp_pts[ipts].imagecross   = wndw_crds[0,ipts]
   temp_pts[ipts].imagealong   = wndw_crds[1,ipts]
   temp_pts[ipts].cross275     = misr_crds[0,ipts]
   temp_pts[ipts].along275     = misr_crds[1,ipts]
   temp_pts[ipts].block        = misr_crds[2,ipts]
   temp_pts[ipts].somcross     = new_som_crds[0,ipts]
   temp_pts[ipts].somalong     = new_som_crds[1,ipts]
   temp_pts[ipts].lon          = lonlat_crds[0,ipts]
   temp_pts[ipts].lat          = lonlat_crds[1,ipts]
   temp_pts[ipts].slope        = new_slope
   temp_pts[ipts].dirfromSomN  = plume_dir_CW_from_som_N
   temp_pts[ipts].dirfromTruN  = plume_dir_CW_from_tru_N

ENDFOR

;---------------------------------------------------------------------------
; Destroy the old linked list except for the first point. This is poor.
;---------------------------------------------------------------------------

LListDelLineObj, pHeadPt, 0

;--------------------------------------------------------------------------- 
; Copy the slope and directions from second point into first point also.
;--------------------------------------------------------------------------- 

(*((*pHeadPt).pData)).slope       = temp_pts[1].slope
(*((*pHeadPt).pData)).dirfromSomN = temp_pts[1].dirfromSomN
(*((*pHeadPt).pData)).dirfromTruN = temp_pts[1].dirfromTruN

;--------------------------------------------------------------------------- 
; Loop over the points again and add the data to the new linked list.
;--------------------------------------------------------------------------- 

FOR ipts=1,num_spline_pts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Create the new linked list and copy the data in.
   ;------------------------------------------------------------------------

   !VAR.LinkList.pThisPoint = LListInsertTail(pHeadPt, $
                 !KON.NodeObjTyp.LINEPT_NODE, temp_pts[ipts])
ENDFOR

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

retval = 0

CleanItUp:

som_coords = 0
image_coords = 0
misr_coords = 0
lonlat_coords = 0
new_som_crds = 0
lonlat_crds = 0
misr_crds = 0
wndw_crds = 0
temp_pts = 0

END  ;  ResampleDirLine

;***************************************************************************
PRO GetPointsInRegion, State, PixSep, pHeadObj, pThisRgn, NumGoodPts
;***************************************************************************
; Given a region circumscribed by points, find a collection of more or less
; equally spaced points inside this region.  If the wind direction is known,
; then for each point in the region, find the nearest line direction point
; in the linked list and use its wind direction.  Also, replace the line
; direction list with the region point list.  Save the original direction
; line MISR point coordinates in a special list in the region object. Also,
; associate data from MODIS fire pixels with individual pixels in the region.
;---------------------------------------------------------------------------

COMMON data_structs, region_data, linept_data

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumGoodPts = 0

;---------------------------------------------------------------------------
; If the linked list contains points with wind directions, copy the SOM
; coordinates and wind direction data out.
;---------------------------------------------------------------------------

pHeadPt = (*pThisRgn).pNextLinePt

IF (!SAV.Digitize.WIND_TYPE GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Construct an array of structures for storing slope information from
   ; original direction line.
   ;------------------------------------------------------------------------

   pt_str = {somx:0.0, somy:0.0, slope:0.0, $
             plume_dir_CW_from_som_N:0.0, plume_dir_CW_from_tru_N:0.0}
   num_pts = LListCount(pHeadPt)
   pts_list = REPLICATE(pt_str, num_pts)
   icnt = 0
   pNextPt = pHeadPt

   ;------------------------------------------------------------------------
   ; Construct an array for storing MISR coordinates from original direction
   ; line.
   ;------------------------------------------------------------------------

   coord_list = INTARR(3,num_pts)
   
   ;------------------------------------------------------------------------
   ; Loop over the points.
   ;------------------------------------------------------------------------

   WHILE (PTR_VALID(pNextPt) AND icnt LT num_pts) DO BEGIN

      IF (PTR_VALID((*pNextPt).pData)) THEN BEGIN

         ;------------------------------------------------------------------ 
         ; Copy out the slope data.
         ;------------------------------------------------------------------ 

         PtData = *((*pNextPt).pData)
         pts_list[icnt].somx  = PtData.somcross
         pts_list[icnt].somy  = PtData.somalong
         pts_list[icnt].slope = PtData.slope
         pts_list[icnt].plume_dir_CW_from_som_N = PtData.dirfromSomN
         pts_list[icnt].plume_dir_CW_from_tru_N = PtData.dirfromTruN

         ;------------------------------------------------------------------ 
         ; Copy just the window coordinates of the original linked list into
         ; the direction points array in the region object that owns this
         ; list. This will be written to the text file.
         ;------------------------------------------------------------------

         coord_list[0,icnt] = PtData.cross275
         coord_list[1,icnt] = PtData.along275
         coord_list[2,icnt] = PtData.block

         icnt += 1

      ENDIF
      pNextPt = (*pNextPt).pNextSib

   ENDWHILE

   ;------------------------------------------------------------------------ 
   ; Save a pointer to the array of MISR direction point coordinates.
   ;------------------------------------------------------------------------ 

   (*((*pThisRgn).pData)).direc_pts_spln = PTR_NEW(coord_list)

ENDIF

;--------------------------------------------------------------------------- 
; Destroy the old center line linked list except for the first point.
;---------------------------------------------------------------------------

LListDelLineObj, pHeadPt, 0

;---------------------------------------------------------------------------
; Collect the SOM coordinates from the input linked list of points on the
; periphery of the region.
;---------------------------------------------------------------------------

pHeadPt = (*pThisRgn).pNextPolyPt

LListGetCoordArray, State.sizey, pHeadPt, num_pts, som_coords, $
                    image_coords, misr_coords, lonlatCoords

;---------------------------------------------------------------------------
; Find the point with the minimum x/y coordinates of the peripheral points
; of the region. This is the seed point location for all the remaining points
; inside the rectangle that circumscribes the digitized polygon. It is the
; lower left point in the polygon as viewed on screen.
;---------------------------------------------------------------------------

minx = MIN(som_coords[0,*])
miny = MIN(som_coords[1,*])

;---------------------------------------------------------------------------
; Find the maximum of the peripheral points of the region and the number of
; points along the rectangle's sides.
;---------------------------------------------------------------------------

maxx = MAX(som_coords[0,*])
maxy = MAX(som_coords[1,*])

numx = CEIL(FLOAT(maxx - minx) / PixSep) > 2
numy = CEIL(FLOAT(maxy - miny) / PixSep) > 2

;---------------------------------------------------------------------------
; Construct an array that represents a rectangle of uniformly-spaced points
; that covers the set of polygon-outline points. Retain only those points
; in the rectangle that are also inside polygon region. 
;---------------------------------------------------------------------------

gridobj = Obj_New('IDLanROI', REFORM(som_coords[0,*]), $
                              REFORM(som_coords[1,*]))
som_ary = FLTARR(2,numx*numy)

num_ok = 0
xbeg = minx
FOR ii=0,numx-1 DO BEGIN
   ybeg = miny
   FOR jj=0,numy-1 DO BEGIN
      IF (gridobj->ContainsPoints(xbeg+ii*PixSep, $
                                  ybeg+jj*PixSep)) THEN BEGIN
         som_ary[0,num_ok] = xbeg + ii * PixSep
         som_ary[1,num_ok] = ybeg + jj * PixSep
         num_ok += 1
      ENDIF
   ENDFOR
ENDFOR

Obj_Destroy, gridobj

NumGoodPts = num_ok

;---------------------------------------------------------------------------
; If there are fewer than 2 good points in the region, delete the region and
; ask the user to try again.
;---------------------------------------------------------------------------

pHeadPt = (*pThisRgn).pNextLinePt

IF (NumGoodPts LE 2) THEN BEGIN
   p2ndpt = PTR_NEW()
   IF (PTR_VALID(pHeadPt)) THEN p2ndpt = (*pHeadPt).pNextSib

   IF (~ PTR_VALID(p2ndpt)) THEN BEGIN
      err_msg = ['You must close the polygon for the region to be valid.', $
                 'Make sure the region is large enough to contain 3 ' + $
                 'grid points.', 'Your line will be removed.']
      result = DIALOG_MESSAGE(err_msg, /ERROR)
   ENDIF
   LListDelRgnObj, pThisRgn
   GOTO, get_pts_clean
ENDIF

som_ary = som_ary[0:1,0:num_ok-1]

;---------------------------------------------------------------------------
; Convert the image coordinates to additional coordinates and save.
;---------------------------------------------------------------------------

SomCrdToLonlatCrd, State.Curframe, som_ary, lonlat_crds, retval
SomCrdToMisrCrd, State.Curframe, 0, som_ary, misr_crds, retval

; The next lines fix an occasional problem in the projection.
; Test whether SomCrdToMisrCrd is off by 1/2 pixel.

ndxs = WHERE(misr_crds EQ -1, numndxs)
IF (numndxs GT 0) THEN misr_crds[ndxs] = 0
ndxs = 0

MisrCrdToWndwCrd, State.Curframe, misr_crds, wndw_crds, 1, retval

;---------------------------------------------------------------------------
; Construct an array of point structures that will be copied to the linked
; list as it is created.
;---------------------------------------------------------------------------

dummy = 0
iret = InitLinePtData(dummy)

temp_pts = REPLICATE(linept_data, num_ok)

;--------------------------------------------------------------------------
; Loop over all objects. Find the fire pixel line object if there is one.
;--------------------------------------------------------------------------

IF (PTR_VALID(pHeadObj)) THEN BEGIN

   pNextObj = pHeadObj

   WHILE (PTR_VALID(pNextObj)) DO BEGIN

      ;---------------------------------------------------------------------
      ; Test if the next object is a fire pixel. Break out when found.
      ;---------------------------------------------------------------------

      IF ((*pNextObj).ObjType[0] NE !KON.AerObjTyp.FIREPIXEL_OBJ) THEN BEGIN
         pNextObj = (*pNextObj).pNextSib
         CONTINUE
      ENDIF

      BREAK

   ENDWHILE

   pFireObj = pNextObj

ENDIF

;---------------------------------------------------------------------------
; Stuff the data into the structure for all points on the line. 
;---------------------------------------------------------------------------

pixsep1 = ROUND(PixSep / 0.275)
num_firepix = 0

temp_pts[*].type         = !KON.NodeObjTyp.LINEPT_NODE
temp_pts[*].feature_type = 0
temp_pts[*].imagecross   = REFORM(wndw_crds[0,*])
temp_pts[*].imagealong   = REFORM(wndw_crds[1,*])
temp_pts[*].cross275     = REFORM(misr_crds[0,*])
temp_pts[*].along275     = REFORM(misr_crds[1,*])
temp_pts[*].block        = REFORM(misr_crds[2,*])
temp_pts[*].somcross     = REFORM(som_ary[0,*])
temp_pts[*].somalong     = REFORM(som_ary[1,*])
temp_pts[*].lon          = REFORM(lonlat_crds[0,*])
temp_pts[*].lat          = REFORM(lonlat_crds[1,*])

fire_OK = PTR_VALID(pFireObj) AND $
          ((*pThisRgn).ObjType[0] EQ !KON.AerObjTyp.AER_SMOKE_OBJ OR $
           (*pThisRgn).ObjType[0] EQ !KON.AerObjTyp.AER_VOLCASH_OBJ)

wind_OK = (!SAV.Digitize.WIND_TYPE GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ)

new_slope               = !KON.Misc.BADVALUE_REAL
plume_dir_CW_from_som_N = !KON.Misc.BADVALUE_REAL
plume_dir_CW_from_tru_N = !KON.Misc.BADVALUE_REAL

pNextPt = pHeadPt

FOR ipts1=1,num_ok-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; If this is a plume region with direction line info stored in original
   ; line points, then find the nearest line point and copy its direction
   ; into the current region point.
   ;------------------------------------------------------------------------

   IF (wind_OK) THEN BEGIN
      distmin = 9999999.0
      ndxmin = -1
      FOR ipts2=0,icnt-1 DO BEGIN
         dx = temp_pts[ipts1].somcross - pts_list[ipts2].somx
         dy = temp_pts[ipts1].somalong - pts_list[ipts2].somy
         dist = SQRT(FLOAT(dx) * dx + FLOAT(dy) * dy)
         IF (dist LT distmin) THEN BEGIN
            distmin = dist
            ndxmin = ipts2
         ENDIF
      ENDFOR
      new_slope = pts_list[ndxmin].slope
      plume_dir_CW_from_som_N = pts_list[ndxmin].plume_dir_CW_from_som_N
      plume_dir_CW_from_tru_N = pts_list[ndxmin].plume_dir_CW_from_tru_N
   ENDIF

   temp_pts[ipts1].slope       = new_slope
   temp_pts[ipts1].dirfromSomN = plume_dir_CW_from_som_N
   temp_pts[ipts1].dirfromTruN = plume_dir_CW_from_tru_N

   ;------------------------------------------------------------------------
   ; Find if there is a MODIS fire pixel within space owned by this pixel.
   ; If so, add the data from the fire pixel to the region point. First
   ; test if this is the type of region that is allowed to have fire pixels
   ; associated.
   ;------------------------------------------------------------------------

   modis_data = [-99.9, 0.0]

   IF (fire_OK) THEN BEGIN

      CopyFireDataToRegion, pFireObj, pixsep1, temp_pts[ipts1].block, $
                            temp_pts[ipts1].cross275, temp_pts[ipts1].along275, $
                            modis_data, num_firepix, retval
   ENDIF

   temp_pts[ipts1].modis_data = modis_data

   ;------------------------------------------------------------------------
   ; Add the point to the linked list.
   ;------------------------------------------------------------------------

   IF (PTR_VALID(pNextPt)) THEN BEGIN
      tmp = LListNewNode(!KON.NodeObjTyp.LINEPT_NODE, temp_pts[ipts1])
      (*pNextPt).pNextSib = tmp
      pNextPt = tmp
      !VAR.LinkList.pThisPoint = tmp
   ENDIF

ENDFOR

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

get_pts_clean:

pts_list = 0
som_ary = 0
temp_pts = 0
lonlat_crds = 0
misr_crds = 0
image_crds = 0
wndw_crds = 0

END  ;  GetPointsInRegion

;***************************************************************************
PRO SmoothRetrievedValues, LonLatCrds, Z_Values_In, Bad_Z_Value, Z_Values_Out, $
                           NdxsGood, Std_Dev, RMS_Dev, Status
;***************************************************************************
; Derive a smoothed set of retrieved values by applying Delaunay triangulation
; to the normalized locations (X & Y) of successful retrievals, then finding
; the distance**2-weighted mean Z values within each Voronoi polygon. These
; heights become the "filtered" or "smoothed" values that are reported in the
; raw text file's data table. Status returned:
;   -1    = no valid values were found or could not smooth the data
;   [1,3] = fewer than 4 valid values found, so no smoothed values were
;           calculated, but probably OK to use the few valid raw values
;    0    = success so OK to use the smoothed values
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

MAX_TOP_DIFF = 500.0
KM_PER_DEG = 111.3D
MAX_RNDM_KM_VAL = 0.0100D
CENTER_PT_DIST_SQ = 0.150D * 0.150D
MAX_DIST_TO_KEEP = [2.0, 4.0, 8.0]
NODES_REQ_TO_KEEP = 3

Status = -1
pass_count = 0

xyz:
pass_count += 1
RMS_Dev = !KON.Misc.BADVALUE_REAL
Std_Dev = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Don't proceed unless there are enough good points.
;---------------------------------------------------------------------------

input_z_vals = FLOAT(Z_Values_In)

ndxgood = WHERE(Z_Values_In GT Bad_Z_Value, numgood)

IF (numgood LT 5) THEN BEGIN
   Z_Values_Out = Z_Values_In
   NdxsGood = ndxgood
   IF (numgood GT 0) THEN Status = numgood
   ndxgood = 0
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Here's a simple but questionable heuristic for outlier removal:
; If the point with the maximum value is more than 500 m above the next
; highest point, or if the point with the minimum value is more than 500 m
; below the next lowest point, remove them!
;---------------------------------------------------------------------------

IF (numgood GE 2) THEN BEGIN
   num_elem = N_ELEMENTS(input_z_vals)
   temp_ndxs = SORT(input_z_vals)
   temp_vals = input_z_vals[temp_ndxs]
   
   IF ((temp_vals[num_elem-1] - temp_vals[num_elem-2]) GT MAX_TOP_DIFF) THEN BEGIN
      input_z_vals[temp_ndxs[num_elem-1]] = Bad_Z_Value
      ndxgood = WHERE(input_z_vals GT Bad_Z_Value, numgood)
      temp_vals = input_z_vals[SORT(input_z_vals)]
   ENDIF
   
   good_ndxs = WHERE(temp_vals GT Bad_Z_Value, num_good)
   IF (num_good GE 2) THEN BEGIN
      IF (temp_vals[good_ndxs[1]] - temp_vals[good_ndxs[0]] GT MAX_TOP_DIFF) THEN BEGIN
         low_ndx = WHERE(input_z_vals EQ temp_vals[good_ndxs[0]], num_low)
         IF (num_low EQ 1) THEN BEGIN
            input_z_vals[low_ndx] = Bad_Z_Value
            ndxgood = WHERE(input_z_vals GT Bad_Z_Value, numgood)
         ENDIF
      ENDIF
   ENDIF
   
   good_ndxs = 0
   temp_ndxs = 0
   temp_vals = 0
ENDIF

;---------------------------------------------------------------------------
; Find and use only points with good values in Z value array.
;---------------------------------------------------------------------------

Z_Values_Out = input_z_vals
NdxsGood = ndxgood

IF (numgood LT 5) THEN BEGIN
   IF (numgood GT 0) THEN Status = numgood
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Subset the lat/lon arrays with the good indexes. Then convert them to
; distances from their means in degrees and finally convert to distances from
; their means in km. Also compute the appropriate max value for the random
; perturbations (to avoid tesselation failure).
;---------------------------------------------------------------------------

coords_lon = DOUBLE((REFORM(LonLatCrds[0,*]))[ndxgood])
coords_lat = DOUBLE((REFORM(LonLatCrds[1,*]))[ndxgood])

coords_x = (coords_lon - MEAN(coords_lon)) * KM_PER_DEG * COS(coords_lat/!RADEG)
coords_y = (coords_lat - MEAN(coords_lat)) * KM_PER_DEG
coords_lon = 0
coords_lat = 0

seed = !NULL
rndm_km = MAX_RNDM_KM_VAL * RANDOMN(seed, 2, numgood, /DOUBLE)

coords_x += rndm_km[0,*]
coords_y += rndm_km[1,*]
rndm_km = 0

;---------------------------------------------------------------------------
; Perform Delaunay triangulation using the x-y locations of wind-corrected
; height points. Since this must work for regular grids as well as for
; irregular points, we must handle degenerate cases (3+ points in a line
; or 4+ points on a circle). First perturb all the coordinates by a small
; amount so the results aren't affected significantly. Perform the Delaunay
; triangulation and return indices of the points (nodes) on the corners of
; each Voronoi polygon. We do not need the triangle-list. Node-ndx-list
; contains adjacent polygon node indices in ccwise order with the central
; node index first.
;---------------------------------------------------------------------------

tol = MAX_RNDM_KM_VAL * 1.0e-7

TRIANGULATE, coords_x, coords_y, triangle_list, boundary_pts, TOLERANCE=tol, $
             CONNECTIVITY=node_ndx_list

triangle_list = 0
boundary_pts = 0

;---------------------------------------------------------------------------
; Subset the Z array with the good indexes.
;---------------------------------------------------------------------------

heights_in = input_z_vals[ndxgood]
heights_out = heights_in
input_z_vals = 0

;---------------------------------------------------------------------------
; Find the distance-weighted average of the heights for each Voronoi polygon
; and set the "filtered" height at the central node equal to it. Tiles that
; include boundary nodes have the node itself as the first element returned
; in the connectivity list; interior nodes don't have the node included, so
; it must be added. Because large gaps in the spatial distribution can screw
; things up, eliminate any nodes that are more than a critical distance from
; the central node.
;---------------------------------------------------------------------------

FOR inode=0,numgood-1 DO BEGIN
   adj_nodes = node_ndx_list[node_ndx_list[inode]:node_ndx_list[inode+1]-1]
   IF (adj_nodes[0] NE inode) THEN adj_nodes = [inode, adj_nodes]
   num_node = N_ELEMENTS(adj_nodes)
   delta_x = coords_x[adj_nodes] - coords_x[adj_nodes[0]]
   delta_y = coords_y[adj_nodes] - coords_y[adj_nodes[0]]

   xy_dist = SQRT(delta_x * delta_x + delta_y * delta_y)

   ifound = 0
   FOR irmv=0,N_ELEMENTS(MAX_DIST_TO_KEEP)-1 DO BEGIN
      ndx_keep = WHERE(xy_dist LE MAX_DIST_TO_KEEP[irmv], numndx_keep)
      IF (numndx_keep GE NODES_REQ_TO_KEEP) THEN BEGIN
         ifound = 1
         BREAK
      ENDIF
   ENDFOR
   
   IF (numndx_keep EQ 1 OR ifound EQ 0) THEN CONTINUE ; keep single or isolated ht
   IF (numndx_keep GT 1 AND numndx_keep LT num_node) THEN BEGIN
      adj_nodes = adj_nodes[ndx_keep]
      delta_x = coords_x[adj_nodes] - coords_x[adj_nodes[0]]
      delta_y = coords_y[adj_nodes] - coords_y[adj_nodes[0]]
   ENDIF
   
   delta_x *= delta_x
   delta_y *= delta_y
   delta_x[0] = CENTER_PT_DIST_SQ
   delta_y[0] = CENTER_PT_DIST_SQ
   dist_wts = 1.0 / SQRT(delta_x + delta_y)
   
   heights_out[inode] = TOTAL(heights_in[adj_nodes] * dist_wts) / TOTAL(dist_wts)
ENDFOR

Z_Values_Out[ndxgood] = heights_out

;---------------------------------------------------------------------------
; Get the RMS value of the difference between the original heights and the
; new heights as a measure of the uncertainty.
;---------------------------------------------------------------------------

RMS_Dev = SQRT(MEAN((heights_in - heights_out) ^ 2))
Std_Dev = STDDEV(heights_out)

IF (FINITE(RMS_Dev, /NAN)) THEN BEGIN
   mssg = ['Encountered NAN in SmoothRetrievedValues', 'Making 1 more attempt']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   IF (pass_count LT 2) THEN GOTO, xyz
ENDIF

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

coords_x = 0
coords_y = 0
delta_x = 0
delta_y = 0
xy_dist = 0
dist_wts = 0
heights_in = 0
heights_out = 0
ndxgood = 0
adj_nodes = 0
node_ndx_list = 0
ndx_keep = 0

Status = 0

END  ;  SmoothRetrievedValues

;***************************************************************************
PRO PrintDisparities, NumPts, MISRCoords, Disparity
;***************************************************************************
; Prints disparity values for each camera at each point in digitized region.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct

temp_offsets = Disparity
ndxs = WHERE(temp_offsets LT -999.0, numndxs)
IF (numndxs GT 0) THEN temp_offsets[ndxs] = -99.9

PRINTF, !KON.Misc.LogUnit, 'Disparity Results for Orbit : ' + $
                           STRTRIM(STRING(CoordStruct.(0).OrbitNum),2)
PRINTF, !KON.Misc.LogUnit, 'SOM Coordinates for 275 M Pixels'

PRINTF, !KON.Misc.LogUnit, ' '
PRINTF, !KON.Misc.LogUnit, '  Point Block  SOM   SOM   Cam  Disp  Disp'
PRINTF, !KON.Misc.LogUnit, '   Num   Num  Cross Along Name Cross Along'
PRINTF, !KON.Misc.LogUnit, '------- ----- ----- ----- ---- ----- -----'

FOR ipts = LONG(0), NumPts-1 DO BEGIN
   FOR icam = !SAV.Digitize.FIRST_CAM_USE, !SAV.Digitize.LAST_CAM_USE DO BEGIN

      cam_num = !KON.Instr.CAM_ORDER[icam]

      PRINTF, !KON.Misc.LogUnit, FORMAT='(I6,1X,I5,I7,I5,A5,F7.1,F6.1)', ipts+1, $
              MISRCoords[2,ipts], MISRCoords[0,ipts], MISRCoords[1,ipts], $
              !KON.Instr.camera_names[cam_num+1], $
              temp_offsets[0,cam_num,ipts], temp_offsets[1,cam_num,ipts]
   ENDFOR
ENDFOR

temp_offsets = 0
ndxs = 0

END  ;  PrintDisparities

;***************************************************************************
PRO SaveMinMaxValues, InVals, DataType
;***************************************************************************
; Main routine for processing plume heights.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE

minval = MIN(InVals, MAX=maxval)
!VAR.DataRgn.VALUE_MIN_DIG[DataType] = minval
!VAR.DataRgn.VALUE_MAX_DIG[DataType] = maxval
!VAR.DataRgn.VALUE_MIN_DLG[DataType] = minval
!VAR.DataRgn.VALUE_MAX_DLG[DataType] = maxval
IF (minval LT !VAR.DataRgn.VALUE_MIN_ABS[DataType]) THEN $
   !VAR.DataRgn.VALUE_MIN_ABS[DataType] = minval
IF (maxval GT !VAR.DataRgn.VALUE_MAX_ABS[DataType]) THEN $
   !VAR.DataRgn.VALUE_MAX_ABS[DataType] = maxval

END  ;  SaveMinMaxValues

;***************************************************************************
PRO ProcessRegionParams, State, pNextRgn, Retval
;***************************************************************************
; Main routine for processing plume heights.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct
COMMON OpenWindows, WndwStereoPlot, WndwStereoHist, WndwAerosolHist
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles
COMMON xtra_plot_options, SHOW_HTS_NEAR_GROUND

;---------------------------------------------------------------------------
; Initialize counter needed for double-band retrievals.
;---------------------------------------------------------------------------

!SAV.Digitize.TWO_RETRIEVALS = 0

;---------------------------------------------------------------------------
; Initialize other stuff.
;---------------------------------------------------------------------------

Retval = -1
no_more_plots = 0

WndwStereoPlot  = INTARR(2) - 1
WndwStereoHist  = INTARR(2) - 1
WndwAerosolHist = INTARR(2) - 1

whichorbit = (State.curframe GT 9)

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Set up processing options for each region type.
;---------------------------------------------------------------------------

IF (!SAV.Digitize.AER_TYPE EQ !KON.AerObjTyp.AER_NULL_OBJ OR $
    !SAV.Digitize.AER_TYPE GT !KON.AerObjTyp.AER_XTRA2_OBJ) THEN RETURN

DO_CORR_HEIGHTS = 0
IF (!SAV.Digitize.WIND_TYPE GE !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN $
   DO_CORR_HEIGHTS = 1

;---------------------------------------------------------------------------
; Obtain and store the UTC time for this region's first point.
;---------------------------------------------------------------------------

block = (*(*(*pNextRgn).pNextPolyPt).pData).block

cntrtime = STRMID(BlockCntrTime[block-1], 11, 8)

(*((*pNextRgn).pData)).utc_time = cntrtime

;---------------------------------------------------------------------------
; Load land/water mask and digital elevation data for all the blocks that
; are present if not already loaded.
;---------------------------------------------------------------------------

LoadAGPData, State, retval
IF (retval NE 0) THEN BEGIN
   LListDelRgnObj, pNextRgn
   RedrawWindow, State, State.curframe
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Load camera view angle data for all the blocks that are present if not
; already loaded.
;---------------------------------------------------------------------------

LoadGMPData, State, retval
IF (retval NE 0) THEN BEGIN
   LListDelRgnObj, pNextRgn
   RedrawWindow, State, State.curframe
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set up the logging file if requested.
;---------------------------------------------------------------------------

IF (!SAV.Digitize.PRINT_OFFSETS GE 1) THEN BEGIN
   LogFile = !SAV.WorkingDir + (*((*pNextRgn).pData)).name[0] + '_PlumeLog.txt'
   OPENW, !KON.Misc.LogUnit, LogFile, /GET_LUN
ENDIF

;---------------------------------------------------------------------------
; Set the current window to the An image.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

;---------------------------------------------------------------------------
; Fill an array with the coordinates of the digitized region's perimeter.
; Fill another with the coordinates of the points automatically picked in
; the region's interior.
;---------------------------------------------------------------------------

LListGetCoordArray, State.sizey, (*pNextRgn).pNextPolyPt, num_pts_poly, $
                    som_coords_poly, image_coords_poly, misr_coords_poly, $
                    lonlat_coords_poly
LListGetCoordArray, State.sizey, (*pNextRgn).pNextLinePt, num_pts_line, $
                    som_coords_line, image_coords_line, misr_coords_line, $
                    lonlat_coords_line
                    
mean_latitude = TOTAL(lonlat_coords_poly[1,*]) / N_ELEMENTS(lonlat_coords_poly) * 2.0

misr_coords_poly = 0
lonlat_coords_poly = 0

;---------------------------------------------------------------------------
; Compute the perimeter of the smoke cloud in km and area in square km. and
; copy the results to the region linked list node.
;---------------------------------------------------------------------------

line_len = 0.0
rgn_area = 0.0

IF (num_pts_poly GT 2) THEN BEGIN
   line_len = ComputeLineLength(num_pts_poly, som_coords_poly)
   rgn_area = POLY_AREA(som_coords_poly[0,*], som_coords_poly[1,*])
ENDIF
IF (!SAV.Digitize.GEOM_TYPE EQ !KON.GeomObjTyp.GEOM_LINE_OBJ) THEN BEGIN
   line_len = ComputeLineLength(num_pts_line, som_coords_line)
ENDIF

som_coords_poly = 0

(*((*pNextRgn).pData)).area = rgn_area
(*((*pNextRgn).pData)).perimeter = line_len

;---------------------------------------------------------------------------
; Retrieve the slope and direction of the plume per point from point nodes
; on the direction line. This is done only to get direc_somN. direc_truN is
; not used.
;---------------------------------------------------------------------------

LListGetPlumeSlopeDirec, (*pNextRgn).pNextLinePt, num_pts_line, $
                          direc_slope_som, direc_somN, direc_truN, retval

;---------------------------------------------------------------------------
; Load land/water mask data for the points in this region.
;---------------------------------------------------------------------------

GetLandWaterData, State, CoordStruct.(whichorbit).PathNum, num_pts_line, $
                  misr_coords_line, landwater_mask, retval
IF (retval NE 0) THEN RETURN

;---------------------------------------------------------------------------
; Loop over the red and/or blue band retrievals as requested by user. Store
; data retrieved with any band in position 0 except when the double-band
; mode is used. Then store data for the red band in position 0 and data
; retrieved with the blue band in position 1 of the linked list arrays.
;---------------------------------------------------------------------------

beg_band_to_use = (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ $
                   !KON.BandObjTyp.BLUE_BAND) ? 1 : 0
end_band_to_use = (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ $
                   !KON.BandObjTyp.BLUE_BAND OR $
                   !SAV.Digitize.RETRIEVE_BAND_TYPE EQ $
                   !KON.BandObjTyp.BOTH_BAND) ? 1 : 0
ifirst = 1

FOR iBand = beg_band_to_use, end_band_to_use DO BEGIN

   !VAR.DataRgn.SHOW_BAND_TYPE = iBand

   ;------------------------------------------------------------------------
   ; Set the band # to use in retrieving heights if using both red and blue.
   ; !KON.Instr.RED = 0, !KON.Instr.BLU = 2.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.BOTH_BAND) THEN $
      !SAV.Digitize.USE_BAND_NDX = ([!KON.Instr.RED, !KON.Instr.BLU])[iBand]
   
   ;------------------------------------------------------------------------
   ; Construct rectangles around the plume polygon in each camera and
   ; compute fractional pixel along and across disparities using a cross-
   ; correlation method. Compute disparities every few pixels in a profile
   ; along plume direction line or on grid interior to a digitized region.
   ;------------------------------------------------------------------------

   bands = INTARR(num_pts_line)
   disparity_som = FLTARR(2,!KON.Instr.NCAM,num_pts_line) + $
                   !KON.Misc.BADVALUE_REAL

   FindDisparities, State, pNextRgn, num_pts_line, image_coords_line, $
                    landwater_mask, bands, disparity_som, corr_coeffs, retval

   ;------------------------------------------------------------------------
   ; Print disparity results if requested.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.PRINT_OFFSETS EQ 1) THEN BEGIN
      PrintDisparities, num_pts_line, misr_coords_line, disparity_som
      FREE_LUN, !KON.Misc.LogUnit
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Get window coordinates for all the points for each camera where the
   ; disparities occur, i.e. where each camera sees the terrain.
   ; NOTE - returned coordinates have the along-track origin at the TOP!
   ;------------------------------------------------------------------------

   GetCamOffsetCoords, State, num_pts_line, misr_coords_line, $
                       disparity_som, CamOffsetCrds, Retval

   ;------------------------------------------------------------------------
   ; Load digital elevation data for the points in this region.
   ;------------------------------------------------------------------------

   GetTerrainData, State, num_pts_line, CamOffsetCrds, terrain_ht, retval

   ;------------------------------------------------------------------------
   ; Load camera view angle data for the points in this region. If there are
   ; no valid values for the nadir camera, quit.
   ;------------------------------------------------------------------------

   GetGeomData, 2, State, mean_latitude, num_pts_line, CamOffsetCrds, $
                terrain_ht, cam_azimuth_ang, cam_zenith_ang, cam_scatter_ang, $
                cam_glitter_ang, sun_azimuth_ang, sun_zenith_ang, retval

   cam_scatter_ang = 0
   cam_glitter_ang = 0
   sun_azimuth_ang = 0

   ;------------------------------------------------------------------------
   ; Do the following only during the first time through this loop of one or
   ; two iterations to avoid redundant calculations.
   ;------------------------------------------------------------------------

   IF (ifirst) THEN BEGIN

      ;---------------------------------------------------------------------
      ; Test that the geometry data were retrieved correctly. This involves
      ; only the An camera, so it needs to be done only once.
      ;---------------------------------------------------------------------

      ndxs = WHERE(cam_azimuth_ang[4,*] NE !KON.Misc.BADVALUE_REAL, numndxs1)
      ndxs = WHERE(cam_zenith_ang [4,*] NE !KON.Misc.BADVALUE_REAL, numndxs2)
      ndxs = WHERE(sun_zenith_ang [*]   NE !KON.Misc.BADVALUE_REAL, numndxs3)
      IF (numndxs1 EQ 0 OR numndxs2 EQ 0 OR numndxs3 EQ 0) THEN BEGIN
         mssg = ['There are bad values in the geometric parameters for', $
                 'all points in this plume. No heights can be computed.']
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         LListDelRgnObj, pNextRgn
         RedrawWindow, State, State.curframe
         IF (!SAV.Digitize.PRINT_OFFSETS GE 1) THEN FREE_LUN, !KON.Misc.LogUnit
         RETURN
      ENDIF

      ndxs = 0

      ;---------------------------------------------------------------------
      ; Store the terrain heights at the AN camera in the point linked list
      ; node structures for the line. Also create and store terrain height
      ; histograms.
      ;---------------------------------------------------------------------

      ipts = 0
      pNext = (*pNextRgn).pNextLinePt

      WHILE PTR_VALID(pNext) DO BEGIN
         (*((*pNext).pData)).terr_elev  = terrain_ht[4,ipts]
         (*((*pNext).pData)).land_water = landwater_mask[ipts]
         ipts += 1
         pNext = (*pNext).pNextSib
      ENDWHILE

      rgn_name = (*((*pNextRgn).pData)).name[iBand]

      GetArrayMinMax, terrain_ht, !KON.Misc.LARGE_POS_NUM, $
                      !KON.Misc.LARGE_NEG_NUM, min_val, max_val, status

      InitHistogram, !KON.Histo.HEIGHT_HIST, rgn_name, 1, 1, $
                     !KON.Histo.HT_BIN_CNTR, !KON.Histo.HT_BIN_SIZE, $
                     min_val, max_val, TerrHtStruct
      BuildHistogram, 'MINX', 0, 1, terrain_ht, TerrHtStruct

      IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
         LLStoreHistogram, TerrHtStruct, 0, BinSize, NumBins, pCntrAry, pHistAry
         (*((*pNextRgn).pData)).tht_binsize = BinSize
         (*((*pNextRgn).pData)).tht_numbins = NumBins
         (*((*pNextRgn).pData)).terrht_cntr = pCntrAry
         (*((*pNextRgn).pData)).terrht_hist = pHistAry
      ENDIF

      FreeHistStruct, TerrHtStruct

   ENDIF

   ;------------------------------------------------------------------------
   ; At each disparity point for each camera, find the directions from true
   ; geographic north to swath (ground track) north and to SOM north.
   ;------------------------------------------------------------------------

   GetSwathDirRelGeoNorth, State, num_pts_line, CamOffsetCrds, $
                           north_to_som_azim, north_to_swath_azim, retval

   ;------------------------------------------------------------------------
   ; Compute across and along components of zenith angles for each camera
   ; from their azimuth and zenith angles. Rotate so they're relative to SOM
   ; north and swath north.
   ;------------------------------------------------------------------------

   ComputeZenithComponents, cam_azimuth_ang, cam_zenith_ang, num_pts_line, $
                            north_to_som_azim, north_to_swath_azim, $
                            cam_zen_cross_som, cam_zen_along_som, $
                            cam_zen_cross_swath, cam_zen_along_swath, retval

   north_to_som_azim = 0
   north_to_swath_azim = 0
   
   ;------------------------------------------------------------------------
   ; Rotate the points on the direction line so they are defined with
   ; respect to the swath (ground track) direction.
   ;------------------------------------------------------------------------

   RotateDirectionLine, direc_slope_som, direc_slope_swath, direc_somN, $
                        direc_swathN, retval

   ;------------------------------------------------------------------------
   ; Rotate the along and across track disparity components so they are
   ; orthogonal with respect to the swath (ground track) direction.
   ;------------------------------------------------------------------------

   RotateDisparityComponents, disparity_som, disparity_swath, retval
                 
   ;------------------------------------------------------------------------
   ; Store the raw disparities in the point linked list node structures for
   ; the line. Save the min and max values in !VAR first.
   ;------------------------------------------------------------------------
   
   cross_vals = REFORM(disparity_swath[0,*,*])
   ndxs = WHERE(cross_vals GT !KON.Misc.BADVALUE_REAL, numndx)
   IF (numndx GT 0) THEN SaveMinMaxValues, cross_vals[ndxs], $
                                           !KON.DataRgn.TYPE_DISP_CROSS
   along_vals = REFORM(disparity_swath[1,*,*])
   ndxs = WHERE(along_vals GT !KON.Misc.BADVALUE_REAL, numndx)
   IF (numndx GT 0) THEN SaveMinMaxValues, along_vals[ndxs], $
                                           !KON.DataRgn.TYPE_DISP_ALONG
   cross_vals = 0
   along_vals = 0
   ndxs = 0

   ipts = 0
   pNext = (*pNextRgn).pNextLinePt
   
   WHILE PTR_VALID(pNext) DO BEGIN
   
      (*((*pNext).pData)).band_used[iBand] = bands[ipts]
      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         (*((*pNext).pData)).cross_dispar[icam,iBand] = disparity_swath[0,icam,ipts]
         (*((*pNext).pData)).along_dispar[icam,iBand] = disparity_swath[1,icam,ipts]
      ENDFOR
      
      ipts += 1
      pNext = (*pNext).pNextSib
   ENDWHILE
   
   IF (ipts NE num_pts_line) THEN BEGIN
      mssg = 'The number of points counted in line array is not correct.'
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Get the local geocentric radius of the earth.
   ;-----------------------------------------------------------------------

   RadiusLocal = ComputeGeocentricRadius(mean_latitude)
   
   ;------------------------------------------------------------------------
   ; Initialize parameters in linked lists.
   ;-----------------------------------------------------------------------
   
   pNext = (*pNextRgn).pNextLinePt
   
   WHILE PTR_VALID(pNext) DO BEGIN
      (*((*pNext).pData)).zero_wind_ht[iBand] = !KON.Misc.BADVALUE_REAL
      (*((*pNext).pData)).corr_wind_ht[iBand] = !KON.Misc.BADVALUE_REAL
      (*((*pNext).pData)).smooth_hghts[iBand] = !KON.Misc.BADVALUE_REAL
      (*((*pNext).pData)).wnd_speed_cr[iBand] = !KON.Misc.BADVALUE_REAL
      (*((*pNext).pData)).wnd_speed_al[iBand] = !KON.Misc.BADVALUE_REAL
      pNext = (*pNext).pNextSib
   ENDWHILE
   
   ;------------------------------------------------------------------------
   ; START OF ZERO-WIND RETRIEVAL. Use disparities and geometry to compute
   ; above-ellipsoid zero-wind heights for all points. Reverse signs on
   ; camera zenith angle components.
   ;------------------------------------------------------------------------

   pt_hts_zero = FLTARR(num_pts_line)

   ComputeZeroWindHts, num_pts_line, RadiusLocal, disparity_swath, $
                       corr_coeffs, no_more_plots, terrain_ht, $
                       cam_zen_along_swath, pt_hts_zero_cam, pt_hts_zero, $
                       retval
   pt_hts_zero_cam = 0

   ;------------------------------------------------------------------------
   ; Store the zero-wind heights in the linked list. Save min and max values.
   ;------------------------------------------------------------------------

   ndxs = WHERE(pt_hts_zero GT !KON.Misc.BADVALUE_REAL, numndx)
   IF (numndx GT 0) THEN SaveMinMaxValues, pt_hts_zero[ndxs], $
                                           !KON.DataRgn.TYPE_ZEROWIND_HT
   ndxs = 0
   
   ipts = 0
   pNext = (*pNextRgn).pNextLinePt

   WHILE PTR_VALID(pNext) DO BEGIN
      (*((*pNext).pData)).zero_wind_ht[iBand] = pt_hts_zero[ipts]
      ipts += 1
      pNext = (*pNext).pNextSib
   ENDWHILE

   pt_hts_wcorr  = FLTARR(num_pts_line) + !KON.Misc.BADVALUE_REAL
   pt_wind_cross = FLTARR(num_pts_line) + !KON.Misc.BADVALUE_REAL
   pt_wind_along = FLTARR(num_pts_line) + !KON.Misc.BADVALUE_REAL

   IF (~ DO_CORR_HEIGHTS) THEN BEGIN
      
      ;---------------------------------------------------------------------
      ; If wind-corrected heights were not computed, smooth the zero-
      ; wind heights using Delaunay triangulation and median filtering
      ; and store them in the linked list for this object.
      ;---------------------------------------------------------------------

      heights_input = REFORM(pt_hts_zero[*])
      SmoothRetrievedValues, lonlat_coords_line, heights_input, $
                             !KON.Misc.BADVALUE_REAL, heights_smoothed, $
                             ndxs_good, Std_dev_hts, RMS_dev_hts, retval
      ipts = 0
      pNext = (*pNextRgn).pNextLinePt
     
      WHILE PTR_VALID(pNext) DO BEGIN
         (*((*pNext).pData)).smooth_hghts[iBand] = heights_smoothed[ipts]
         ipts += 1
         pNext = (*pNext).pNextSib
      ENDWHILE

   ENDIF ELSE BEGIN
      
      ;---------------------------------------------------------------------
      ; START OF WIND-CORRECTED RETRIEVAL. 
      ;---------------------------------------------------------------------
   
      IF (!SAV.Digitize.WIND_TYPE EQ $
          !KON.WindObjTyp.WIND_USER_DIREC_OBJ) THEN BEGIN
          
         ;------------------------------------------------------------------
         ; Use disparities and geometry to compute wind-corrected heights
         ; and winds for all points. Reverse the signs on camera zenith
         ; angle components.
         ;------------------------------------------------------------------

         no_more_plots = 0
         
         ComputeHeightsAndWinds, $
                RadiusLocal, num_pts_line, lonlat_coords_line, $
                direc_slope_swath, direc_swathN, disparity_swath, $
                corr_coeffs, no_more_plots, terrain_ht, $
                cam_zen_cross_swath, cam_zen_along_swath, $
                pt_hts_wcorr, pt_wind_cross, pt_wind_along, retval
      ENDIF

      ;---------------------------------------------------------------------
      ; Smooth the wind-corrected heights using Delaunay triangulation and
      ; median filtering.
      ;---------------------------------------------------------------------

      heights_input = REFORM(pt_hts_wcorr[*])
      
      SmoothRetrievedValues, lonlat_coords_line, heights_input, $
                             !KON.Misc.BADVALUE_REAL, heights_smoothed, $
                             ndxs_good, Std_dev_hts, RMS_dev_hts, retval
                             
      ;---------------------------------------------------------------------
      ; Store heights and winds in the linked list. Save min and max values.
      ;---------------------------------------------------------------------

      ndxs = WHERE(pt_hts_wcorr GT !KON.Misc.BADVALUE_REAL, numndx)
      IF (numndx GT 0) THEN SaveMinMaxValues, pt_hts_wcorr[ndxs], $
                                              !KON.DataRgn.TYPE_WINDCORR_HT
      ndxs = WHERE(heights_smoothed GT !KON.Misc.BADVALUE_REAL, numndx)
      IF (numndx GT 0) THEN SaveMinMaxValues, heights_smoothed[ndxs], $
                                              !KON.DataRgn.TYPE_SMOOTHED_HT
      ndxs = WHERE(pt_wind_cross GT !KON.Misc.BADVALUE_REAL, numndx)
      IF (numndx GT 0) THEN SaveMinMaxValues, pt_wind_cross[ndxs], $
                                              !KON.DataRgn.TYPE_WIND_CROSS
      ndxs = WHERE(pt_wind_along GT !KON.Misc.BADVALUE_REAL, numndx)
      IF (numndx GT 0) THEN SaveMinMaxValues, pt_wind_along[ndxs], $
                                              !KON.DataRgn.TYPE_WIND_ALONG
      ndxs = WHERE(pt_wind_cross GT !KON.Misc.BADVALUE_REAL AND $
                   pt_wind_along GT !KON.Misc.BADVALUE_REAL, numndx)
      IF (numndx GT 0) THEN BEGIN
          wnd_total = SQRT(pt_wind_cross[ndxs] * pt_wind_cross[ndxs] + $
                           pt_wind_along[ndxs] * pt_wind_along[ndxs])
          SaveMinMaxValues, wnd_total, !KON.DataRgn.TYPE_WIND_TOTAL
          wnd_total = 0
      ENDIF
      ndxs = 0
      
      ipts = 0
      pNext = (*pNextRgn).pNextLinePt

      WHILE PTR_VALID(pNext) DO BEGIN
         (*((*pNext).pData)).corr_wind_ht[iBand] = pt_hts_wcorr[ipts]
         (*((*pNext).pData)).smooth_hghts[iBand] = heights_smoothed[ipts]
         (*((*pNext).pData)).wnd_speed_cr[iBand] = pt_wind_cross[ipts]
         (*((*pNext).pData)).wnd_speed_al[iBand] = pt_wind_along[ipts]
         ipts += 1
         pNext = (*pNext).pNextSib
      ENDWHILE

   ENDELSE

   ;------------------------------------------------------------------------
   ; Compute the best top height and best median height.
   ;------------------------------------------------------------------------

   xCoord = REFORM(image_coords_line[0,*])
   yCoord = REFORM(image_coords_line[1,*])

   GetBestHeights, xCoord, yCoord, heights_input, heights_smoothed, $
                   mean_fltr_hts, bestht_med, bestht_top, status
   
   heights_input = 0

   ;------------------------------------------------------------------------
   ; Display color-coded stereo heights on the An map image. This also
   ; returns the best height (injection height?) for the region. If there
   ; was no best wind-corrected height returned, then try to get a best
   ; zero-wind height. If that fails, just return a median zero-wind height.
   ; Compute the min/max of the heights for map display purposes first.
   ;------------------------------------------------------------------------

   retval = -1

   save_wndw = !D.WINDOW
   SafeWSET, State.draw_win, didit

   samp_spac = (*((*pNextRgn).pData)).samp_spac

   IF (~ DO_CORR_HEIGHTS) THEN BEGIN
      GetHeightMinMax, [pt_hts_zero], [!KON.WindObjTyp.WIND_NO_DIREC_OBJ], $
                       minv, maxv, num_ht_pts, status
      DisplayRetrievedData, image_coords_poly, image_coords_line, $
                            pt_hts_zero, !KON.Colors.ColorVals[3], $
                            samp_spac, !SAV.Digitize.WIND_TYPE, 0, 0, $
                            State.sizex, State.sizey, 1, $
                            bestht_med, bestht_top, retval
   ENDIF
   
   IF (DO_CORR_HEIGHTS) THEN BEGIN
      GetHeightMinMax, [pt_hts_wcorr], $
                       [!KON.WindObjTyp.WIND_USER_DIREC_OBJ], $
                       minv, maxv, num_ht_pts, status
      DisplayRetrievedData, image_coords_poly, image_coords_line, $
                            pt_hts_wcorr, !KON.Colors.ColorVals[1], $
                            samp_spac, !SAV.Digitize.WIND_TYPE, 0, 0, $
                            State.sizex, State.sizey, 1, $
                            bestht_med, bestht_top, retval
   ENDIF

   SafeWSET, save_wndw, didit
   
   ;------------------------------------------------------------------------
   ; Make the contour button sensitive and active.
   ;------------------------------------------------------------------------

   IF (retval EQ 0) THEN BEGIN
      State.showmapclr = 1
      WIDGET_CONTROL, State.wFramesPixColorBtn, SENSITIVE = 1
      WIDGET_CONTROL, State.wFramesPixColorBtn, SET_BUTTON = 1
   ENDIF

   ;------------------------------------------------------------------------
   ; Compute the plume quality factor.
   ;------------------------------------------------------------------------
   
   num_ht_pnts = N_ELEMENTS(ndxs_good)
   
   IF (num_ht_pnts GT 0) THEN BEGIN
      area_per_pt = samp_spac * samp_spac
      fraction_area = ((num_ht_pnts * area_per_pt) / rgn_area) < 1.0
      terr_hts = REFORM(terrain_ht[4,*])
      ndxs1 = WHERE(terr_hts NE !KON.Misc.BADVALUE_REAL, numndxs1)
      max_asl_ht = (numndxs1 GT 0) ? bestht_top - MEAN(terr_hts[ndxs1]) : $
                   bestht_top
      terr_hts = 0

      ComputePlumeQuality, num_ht_pnts, fraction_area, RMS_dev_hts, $
                           max_asl_ht, auto_quality
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Copy the best heights etc. into the linked list and clean up.
   ;------------------------------------------------------------------------

   (*((*pNextRgn).pData)).bestht_med[iBand] = bestht_med
   (*((*pNextRgn).pData)).bestht_top[iBand] = bestht_top
   (*((*pNextRgn).pData)).rms_ht_dev[iBand] = RMS_dev_hts
   (*((*pNextRgn).pData)).auto_quality[iBand] = auto_quality

   ;------------------------------------------------------------------------
   ; Create and display stereo height and wind profiles and histograms.
   ;------------------------------------------------------------------------

   terrain_An_hts = REFORM(terrain_ht[4,*])
   DisplayHeightWindData, State, CoordStruct, pNextRgn, iBand, $
                          num_pts_line, misr_coords_line, $
                          lonlat_coords_line, pt_hts_zero, pt_hts_wcorr, $
                          bestht_med, bestht_top, terrain_An_hts, $
                          pt_wind_cross, pt_wind_along, auto_quality, $
                          retval

   IF (0) THEN BEGIN
      IF (DO_CORR_HEIGHTS) THEN BEGIN
         zero_hts = pt_hts_zero
         corr_hts = heights_smoothed
      ENDIF ELSE BEGIN
         zero_hts = heights_smoothed
         corr_hts = pt_hts_wcorr
      ENDELSE

      DisplayHeightWindData, State, CoordStruct, pNextRgn, iBand, num_pts_line, $
                             misr_coords_line, lonlat_coords_line, zero_hts, $
                             corr_hts, bestht_med, bestht_top, terrain_An_hts, $
                             pt_wind_cross, pt_wind_along, auto_quality, retval
      zero_hts = 0
      corr_hts = 0
   ENDIF

   terrain_An_hts = 0
   heights_smoothed = 0
   
   ;------------------------------------------------------------------------
   ; Save images of the digitized plume if requested: An with boundary pnts,
   ; direction line and fire pixels; An with heights colored; 9 camera
   ; images in 1 MP4 animation or in 9 JPEGs. Include markers and geo grid
   ; if they are turned on.
   ;------------------------------------------------------------------------

   SafeWDELETE, WndwStereoHist[iBand], didit
   WndwStereoHist[iBand] = -1
   
   SavePlumeImages, State, pNextRgn, iBand

   ;------------------------------------------------------------------------
   ; Create and display aerosol histograms and save the histogram data in
   ; the linked list for this region object.
   ;------------------------------------------------------------------------
   
   IF (ifirst AND !SAV.Digitize.DRAW_AEROSOL) THEN BEGIN
      
      DisplayAerosolData, State, CoordStruct, pNextRgn, iBand, $
         num_pts_line, misr_coords_line, $
         som_coords_line, lonlat_coords_line, $
         !SAV.Digitize.DRAW_AEROSOL, retval
         
      SafeWDELETE, WndwAerosolHist[iBand], didit
      WndwAerosolHist[iBand] = -1
   ENDIF
      
   ;------------------------------------------------------------------------
   ; Save data for one or two retrievals for writing to file below.
   ;------------------------------------------------------------------------

   IF (!SAV.Digitize.TWO_RETRIEVALS EQ 0) THEN BEGIN
      pRgnPntr1 = pNextRgn
      iband1 = iBand
   ENDIF ELSE BEGIN
      pRgnPntr2 = pNextRgn
      iband2 = iBand
   ENDELSE

   !SAV.Digitize.TWO_RETRIEVALS += 1

   ifirst = 0

ENDFOR

IF (!SAV.Digitize.RETRIEVE_BAND_TYPE EQ !KON.BandObjTyp.BOTH_BAND) THEN $
   !VAR.DataRgn.SHOW_BAND_TYPE = 2
    
;---------------------------------------------------------------------------
; Display a rotatable cube of height data if requested.
;---------------------------------------------------------------------------

IF ((!SAV.Digitize.SHOW_3D_HEIGHTS AND 1) GT 0) THEN BEGIN

   IF ((!SAV.Digitize.GEOM_TYPE EQ !KON.GeomObjTyp.GEOM_POLYGON_OBJ) AND $
       DO_CORR_HEIGHTS) THEN BEGIN
      ht_type = 2
      old_pt_heights = pt_hts_wcorr
   ENDIF ELSE BEGIN
      ht_type = 1
      old_pt_heights = pt_hts_zero
   ENDELSE

   ;------------------------------------------------------------------------
   ; Load digital elevation data for all the points in a rectangle enclosing
   ; this region.
   ;------------------------------------------------------------------------

   minsamp = MIN(image_coords_line[0,*], MAX=maxsamp)
   modsamp = (maxsamp - minsamp) MOD 4
   IF (modsamp NE 0) THEN maxsamp += (4 - modsamp)
   delsamp = (maxsamp - minsamp) / 4 + 1

   minline = MIN(image_coords_line[1,*], MAX=maxline)
   modline = (maxline - minline) MOD 4
   IF (modline NE 0) THEN maxline += (4 - modline)
   delline = (maxline - minline) / 4 + 1

   new_numpts = delsamp * delline
   new_wndw_coords = INTARR(2,new_numpts)
   new_offsets = FLTARR(2,9,new_numpts) * 0.0
   point_heights = FLTARR(new_numpts) - 999.0

   pt_num = 0
   FOR iline=minline,maxline,4 DO BEGIN
      FOR isamp=minsamp,maxsamp,4 DO BEGIN
         new_wndw_coords[0,pt_num] = isamp
         new_wndw_coords[1,pt_num] = iline
         pt_num += 1
      ENDFOR
   ENDFOR

   npts = N_ELEMENTS(image_coords_line[0,*])
   FOR ipt=0,npts-1 DO BEGIN
      pt_num = (image_coords_line[1,ipt] - minline) / 4. * delsamp + $
               (image_coords_line[0,ipt] - minsamp) / 4.
      point_heights[pt_num] = old_pt_heights[ipt]
   ENDFOR

   WndwCrdToMisrCrd, State.Curframe, new_wndw_coords, new_misr_coords, retval

   GetTerrainData, State, new_numpts, new_misr_coords, new_offsets, $
                   new_terrain_ht, retval

   An_terrain_hts = REFORM(new_terrain_ht[4,*])

   ;------------------------------------------------------------------------
   ; Display the 3D data. This is a modal window, so nothing else can be
   ; done until it is closed.
   ;------------------------------------------------------------------------

   Display3DHeights, (*((*pNextRgn).pData)).name[0], new_wndw_coords, $
                     An_terrain_hts, point_heights, ht_type

   old_pt_heights = 0
   new_wndw_coords = 0
   new_misr_coords = 0
   new_terrain_ht = 0
   new_offsets = 0
   point_heights = 0
   An_terrain_hts = 0
ENDIF

CATCH, /CANCEL

;---------------------------------------------------------------------------
; If the log file is open, close it.
;---------------------------------------------------------------------------

IF (!SAV.Digitize.PRINT_OFFSETS GE 1) THEN FREE_LUN, !KON.Misc.LogUnit

;---------------------------------------------------------------------------
; Destroy the data windows if user OKs it. If this is a smoke "plume", then
; also ask if there is a pyro-cumulus cloud etc. digitized with the smoke.
; In either case, add a space for user comments.
;---------------------------------------------------------------------------

PlumeUserInput_new, State.wTopWorkBase, has_pyrocum, quality, comments, yesno

IF (yesno EQ 'Yes') THEN BEGIN
   FOR iBand=0,1 DO BEGIN
      SafeWDELETE, WndwStereoPlot[iBand], didit
      WndwStereoPlot[iBand] = -1
      SafeWDELETE, WndwAerosolHist[iBand], didit
      WndwAerosolHist[iBand] = -1
   ENDFOR
ENDIF

;---------------------------------------------------------------------------
; Find the lon and lat of the fire pixel with largest radiative firepower.
; If there is no fire pixel, then use the location of the first point
; digitized. Use this to get the biome type code for the fire location.
;---------------------------------------------------------------------------

max_power  = 0
biome_lon  = 0.0
biome_lat  = 0.0
biome_elev = 0.0

icount = 0
pNext = (*pNextRgn).pNextLinePt
WHILE PTR_VALID(pNext) DO BEGIN
   fpower = (*((*pNext).pData)).modis_data[0]
   IF (icount EQ 0 OR fpower GT max_power) THEN BEGIN
      max_power  = fpower
      biome_lon  = (*((*pNext).pData)).lon
      biome_lat  = (*((*pNext).pData)).lat
      biome_elev = (*((*pNext).pData)).terr_elev
   ENDIF
   pNext = (*pNext).pNextSib
   icount += 1
ENDWHILE

(*((*pNextRgn).pData)).biome_lon  = biome_lon
(*((*pNextRgn).pData)).biome_lat  = biome_lat
(*((*pNextRgn).pData)).biome_elev = biome_elev
(*((*pNextRgn).pData)).biome_type = GetBiomeClassAtLatLong(biome_lon, biome_lat)

;---------------------------------------------------------------------------
; Read in the geographic coordinates of the global region polygons and
; construct region objects for testing inclusion.
;---------------------------------------------------------------------------

NumGeoRgn = 7
GetGeoRegionParams, NumGeoRgn, poly_ptr, geo_rgn_names, rgn_colr, GeoRgnParams
rgn_colr = 0
GeoRgnParams = 0

rgn_obj = OBJARR(NumGeoRgn)

FOR irgn=0,NumGeoRgn-1 DO BEGIN
   rgn_obj[irgn] = Obj_New('IDLanROI', $
                          (*(poly_ptr[irgn]))[1,*], (*(poly_ptr[irgn]))[0,*])
   PTR_FREE, poly_ptr[irgn]
ENDFOR

poly_ptr = 0

;---------------------------------------------------------------------------
; Find the geographic region at the fire location and update the linked
; list for this plume.
;---------------------------------------------------------------------------

FOR irgn=0,NumGeoRgn-1 DO BEGIN
   IF (rgn_obj[irgn]->ContainsPoints([biome_lon, biome_lat])) THEN BEGIN
      (*((*pNextRgn).pData)).geo_region = geo_rgn_names[irgn] + $
                     ', ' + STRING(FORMAT='(I1)',irgn)
      BREAK
   ENDIF
ENDFOR

OBJ_DESTROY, rgn_obj

(*((*pNextRgn).pData)).red_blu_pref = '    '

;---------------------------------------------------------------------------
; Save user's info to the region's linked list node and write data to file.
; If the double-band retrieval was done, write both text files here.
;---------------------------------------------------------------------------

(*((*pNextRgn).pData)).user_pyrocum = has_pyrocum
(*((*pNextRgn).pData)).user_comment = comments

Op_WriteRetrievedData, State, State.curframe, pRgnPntr1, iband1, retval
IF (!SAV.Digitize.TWO_RETRIEVALS EQ 2) THEN $
   Op_WriteRetrievedData, State, State.curframe, pRgnPntr2, iband2, retval

;---------------------------------------------------------------------------
; Reset the window to An and redraw.
;---------------------------------------------------------------------------

IF (State.curframe NE 5) THEN ResetFrame, State, 5, 1

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

image_coords_poly = 0
som_coords_line = 0
image_coords_line = 0
misr_coords_line = 0
lonlat_coords_line = 0
direc_slope_som = 0
direc_slope_swath = 0
direc_somN = 0
direc_swathN = 0
landwater_mask = 0
bands = 0
disparity_som = 0
disparity_swath = 0
cam_zen_cross_som = 0
cam_zen_along_som = 0
cam_zen_cross_swath = 0
cam_zen_along_swath = 0
cam_new_zenith = 0
RadiusLocal = 0
CamOffsetCrds = 0
terrain_ht = 0
north_to_swath_azim = 0
cam_azimuth_ang = 0
cam_zenith_ang = 0
sun_zenith_ang = 0
pt_hts_zero = 0
pt_hts_wcorr = 0
pt_hts_fltrd = 0
pt_wind_cross = 0
pt_wind_along = 0

END  ;  ProcessRegionParams
