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
;***************************************************************************
; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
; CODE NEEDS TO BE REORGANIZED. 
;***************************************************************************
;***************************************************************************

;***************************************************************************
PRO LoadBrfConvertData, State, Retval
;***************************************************************************
; Load BRF conversion factors corresponding to all the blocks that are
; currently loaded.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; If BRF data are already loaded, don't repeat. Set hourglass cursor.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.BRF_Loaded) THEN RETURN

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; If this was called for an orbit that was loaded from a .sav file, the file
; names of the L1 files are not available. Get them if possible.
;---------------------------------------------------------------------------

nlen = STRLEN(!VAR.CurrFiles.CamFiles[4])
IF (STRMID(!VAR.CurrFiles.CamFiles[4], nlen-4, 4) EQ '.sav') THEN BEGIN
   cam_files = [!VAR.CurrFiles.NadirFile,'','','','','','','','']
   cam_mask = [1,1,1,1,1,1,1,1,1]
   GetCamFiles, 0, cam_files, CoordStruct.(whichorbit).OrbitNum, 0, 9, $
                whichorbit, cam_mask, Retval
   !VAR.CurrFiles.CamFiles = cam_files
END

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Loop over the 9 L1B2 files and the 4 bands.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   FOR iband=0,!KON.Instr.NBAND-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Read the data in this region for this camera for 4 bands.
      ;---------------------------------------------------------------------

      OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
      IF (iband EQ 0) THEN OpenShut[0] = !KON.ProdTyp.OPEN_F
      IF (iband EQ !KON.Instr.NBAND-1) THEN OpenShut[1] = !KON.ProdTyp.SHUT_F

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.CamFiles[icam], $
                   'BRF_CONV', iband, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   brf_factors, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for BRF conversion factors'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup111
      ENDIF

      brftype = !KON.ProdTyp.PROD_BRF_CONV
      BadValues = !KON.ProdTyp.ProdParms[brftype].BadValues[iband]
      ndxs = WHERE(brf_factors LE BadValues, numndxs)
      IF (numndxs GT 0) THEN brf_factors[ndxs] = !KON.Misc.BADVALUE_REAL

      ;---------------------------------------------------------------------
      ; After reading the first field for one camera, get array size and
      ; build the array.
      ;---------------------------------------------------------------------

      IF (icam EQ 0 AND iband EQ 0) THEN BEGIN
         sizex = (SIZE(brf_factors))[1]
         sizey = (SIZE(brf_factors))[2]
         BrfConvert_Fctrs = FLTARR(sizex, sizey, !KON.Instr.NCAM, $
                                                 !KON.Instr.NBAND)
      ENDIF

      BrfConvert_Fctrs[*,*,icam,iband] = brf_factors

   ENDFOR
ENDFOR

;---------------------------------------------------------------------------
; Get the solar irradiance per band - just use the An camera.
;---------------------------------------------------------------------------

grid_name = !KON.Instr.BAND_NAMES + 'Band'
attr_name = 'std_solar_wgted_height'

SolarIrradiance = FLTARR(!KON.Instr.NBAND) + !KON.Misc.BADVALUE_REAL

grid_id = EOS_GD_OPEN(!VAR.CurrFiles.CamFiles[4])

FOR iband=0,!KON.Instr.NBAND-1 DO BEGIN
   gr_id   = EOS_GD_ATTACH(grid_id, grid_name[iband])
   status  = EOS_GD_READATTR(gr_id, attr_name, val)
   status  = EOS_GD_DETACH(gr_id)
   SolarIrradiance[iband] = val[0]
ENDFOR
   
status  = EOS_GD_CLOSE(grid_id)

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again.
;---------------------------------------------------------------------------

!VAR.CurrFiles.BRF_Loaded = 1
ndxs = 0
brf_factors = 0
Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup111:
ndxs = 0
brf_factors = 0
BrfConvert_Fctrs = 0
SolarIrradiance = 0
SunDistance = 0

END  ;  LoadBrfConvertData

;***************************************************************************
PRO GetBrfConvertData, State, PathNum, NumPts, MisrCoords, BrfFactors, $
                       SolarIrrad, Retval
;***************************************************************************
; Get the BRF conversion factors data corresponding to the points passed in
; from the pre-loaded BRF conversion factor buffer.
;---------------------------------------------------------------------------

COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; Get BRF conversion factors for all points at the determined offset for
; each camera and band.
;---------------------------------------------------------------------------

BrfFactors = FLTARR(!KON.Instr.NCAM, !KON.Instr.NBAND, NumPts) + $
                    !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_BRF_CONV].Dims
cross_ratio = 2048 / dims[0]
along_ratio =  512 / dims[1]

;---------------------------------------------------------------------------
; Loop over the points first, then cameras and bands which all have the
; same point coordinates.
;---------------------------------------------------------------------------

FOR ipt=0L,NumPts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Convert points from block, line, sample to window coordinates. Window
   ; coordinates are the same as "assembled" block coordinates except that
   ; along-track coordinates are inverted (bottom-to-top vs top-to-bottom),
   ; so don't do it again in MisrCrdToWndwCrd (Invert = 0).
   ;------------------------------------------------------------------------

   MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

   sizes = SIZE(BrfConvert_Fctrs)

   ;------------------------------------------------------------------------
   ; Convert coordinates to the correct resolution and invert them to give
   ; assembled 275 m coords. Line and sample coords are 0-based. 
   ;------------------------------------------------------------------------

   IF (MisrCoords[0,ipt] GT -999 AND MisrCoords[1,ipt] GT -999) THEN BEGIN
      samp = FLOOR(wndw_crds[0,ipt] / cross_ratio) > 0
      samp = samp < (sizes[1] - 1)
      line = FLOOR(wndw_crds[1,ipt] / along_ratio) > 0
      line = line < (sizes[2] - 1)
   ENDIF

   ;------------------------------------------------------------------------
   ; Copy the correct BRF values into the array to be returned.
   ;------------------------------------------------------------------------

   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
      FOR iband=0,!KON.Instr.NBAND-1 DO BEGIN
         BrfFactors[icam,iband,ipt] = BrfConvert_Fctrs[samp,line,icam,iband]
      ENDFOR
   ENDFOR

ENDFOR

;---------------------------------------------------------------------------
; Get the solar irradiance per band.
;---------------------------------------------------------------------------

SolarIrrad = SolarIrradiance

END  ;  GetBrfConvertData

;***************************************************************************
PRO LoadAGPData, State, Retval
;***************************************************************************
; Load digital elevation data corresponding to all the blocks that are
; currently loaded.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON agp_data, Land_Water_Mask, Terrain_Hts

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; If AGP data are already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.AGP_Loaded) THEN GOTO, try_biome

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set the hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Get the filename for AGP data for this MISR path.
;---------------------------------------------------------------------------

path_str = STRTRIM(STRING(CoordStruct.(whichorbit).PathNum),2)
IF (STRLEN(path_str) LT 3) THEN path_str = '0' + path_str
IF (STRLEN(path_str) LT 3) THEN path_str = '0' + path_str
path_str = 'P' + path_str

file_filter = ['MISR*_AGP_*' + path_str + '_*.hdf']
temp_AGPfile = !VAR.CurrFiles.AGPfile
GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeAgp, $
                 file_filter, 0, file_outpath, temp_AGPfile
!VAR.CurrFiles.AGPfile = temp_AGPfile
IF (!VAR.CurrFiles.AGPfile EQ '') THEN BEGIN
   Retval = -2
   RETURN
ENDIF

IF (~ FILE_TEST(!VAR.CurrFiles.AGPfile)) THEN BEGIN
   mssg = ['AGP file not valid:', !VAR.CurrFiles.AGPfile]
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Read the land-water mask.
;       0 = Shallow Ocean, 1 = Land, 2 = Coastline,
;       3 = Shallow Inland Water, 4 = Ephemeral Water,
;       5 = Deep Inland Water, 6 = Deep Ocean
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AGPfile, 'AGP', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Land_Water_Mask, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for AGP Land-Water mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   Land_Water_Mask = 0
   !VAR.CurrFiles.AGP_Loaded = 0
   RETURN
ENDIF

Land_Water_Mask = FLOAT(Land_Water_Mask)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP].BadValues[0]
ndxs = WHERE(Land_Water_Mask LE BadValues, numndxs)
IF (numndxs GT 0) THEN Land_Water_Mask[ndxs] = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Read the terrain heights.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AGPfile, 'AGP', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Terrain_Hts, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for AGP Terrain height'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ndxs = 0
   Land_Water_Mask = 0
   Terrain_Hts = 0
   !VAR.CurrFiles.AGP_Loaded = 0
   RETURN
ENDIF

Terrain_Hts = FLOAT(Terrain_Hts)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP].BadValues[1]
ndxs = WHERE(Terrain_Hts LE BadValues, numndxs)
IF (numndxs GT 0) THEN Terrain_Hts[ndxs] = !KON.Misc.BADVALUE_REAL
ndxs = 0

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again.
;---------------------------------------------------------------------------

!VAR.CurrFiles.AGP_Loaded = 1

;---------------------------------------------------------------------------
; Always load the biome grid when the AGP is loaded to ensure it will always
; be available when digitizing plumes, because MINX needs to write the biome
; type to the output text file.
; Return the Biome retval flag to caller to cancel retrieval if needed, but
; keep the AGP_Loaded setting above to avoid repeating that load.
;---------------------------------------------------------------------------

try_biome: LoadBiomeData, State, Retval

END  ;  LoadAGPData

;***************************************************************************
PRO GetCamOffsetCoords, State, NumPts, MisrCoords, Offsets, CamOffsetCrds, $
                        Retval
;***************************************************************************
; Given the input block/line/samp coords for each An camera point and the
; disparity offsets for each camera, find the coordinates for each camera's
; offset at each point. Return them in window coordinates, because that's
; the system in which the terrain and geometry data are stored after loading.
; Window coordinates are the same as "assembled" block coordinates except
; along-track coordinates are inverted (bottom-to-top vs top-to-bottom).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Allocate the array or window coordinates to return.
;---------------------------------------------------------------------------

CamOffsetCrds = FLTARR(2,!KON.Instr.NCAM,NumPts) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Convert all points from block/line/sample to line/sample window coords.
; Don't invert the along-track coordinates (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval
   
;---------------------------------------------------------------------------
; Loop over the cameras and initialize the values in the output array with
; the An camera's coordinates. Also get the camera-order index values.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   new_offsets = REFORM(Offsets[*,icam,*])

   ;------------------------------------------------------------------------
   ; Initialize each camera's window coordinate values in the output array
   ; with the An camera's values plus the offset from An for the current
   ; camera as determined by image matching. Use camera-order index values.
   ;------------------------------------------------------------------------

   temp_coords = wndw_crds + new_offsets
   
   ;------------------------------------------------------------------------
   ; Replace coordinates with bad value indicator if any offset was bad or
   ; out-of-range.
   ;------------------------------------------------------------------------
   
   ndxs = WHERE(new_offsets EQ !KON.Misc.BADVALUE_REAL, numbad)
   IF (numbad GT 0) THEN temp_coords[ndxs] = !KON.Misc.BADVALUE_REAL

   tempx = REFORM(temp_coords[0,*])
   tempy = REFORM(temp_coords[1,*])
 
   ndxs = WHERE(tempx GT !KON.Misc.BADVALUE_REAL AND $
                tempx LT 0.5 OR tempx GE State.sizex - 0.5, numbad)
                
   IF (numbad GT 0) THEN BEGIN
      tempx[ndxs] = !KON.Misc.BADVALUE_REAL
      tempy[ndxs] = !KON.Misc.BADVALUE_REAL
   ENDIF

   ndxs = WHERE(tempy GT !KON.Misc.BADVALUE_REAL AND $
                tempy LT 0.5 OR tempy GE State.sizey - 0.5, numbad)
                
   IF (numbad GT 0) THEN BEGIN
      tempx[ndxs] = !KON.Misc.BADVALUE_REAL
      tempy[ndxs] = !KON.Misc.BADVALUE_REAL
   ENDIF

   temp_coords[0,*] = tempx
   temp_coords[1,*] = tempy

   CamOffsetCrds[*,icam,*] = temp_coords

   ;------------------------------------------------------------------------
   ; Reset the coordinates in the output array for the An camera with the
   ; input values.
   ;------------------------------------------------------------------------

   IF (icam EQ !KON.Instr.AN) THEN CamOffsetCrds[*,icam,*] = wndw_crds

ENDFOR

;---------------------------------------------------------------------------
; Clean up and return..
;---------------------------------------------------------------------------

ndxs = 0
wndw_crds = 0
temp_coords = 0
new_offsets = 0

Retval = 0

END  ;  GetCamOffsetCoords

;***************************************************************************
PRO GetTerrainData, State, NumPts, CamCoords, TerrainHt, Retval
;***************************************************************************
; Get the terrain data corresponding to the points passed in from the pre-
; loaded AGP buffer.
;---------------------------------------------------------------------------

COMMON agp_data, Land_Water_Mask, Terrain_Hts

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the size of the entire image in "assembled" window coordinates at
; Terrain_Hts resolution and get the resolution coversion factors.
;---------------------------------------------------------------------------

sizes = SIZE(Terrain_Hts)

dims = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP].Dims
res_ratio = FLTARR(2)
res_ratio[0] = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims[0]
res_ratio[1] = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims[1]

;---------------------------------------------------------------------------
; Allocate the array to return.
;---------------------------------------------------------------------------

TerrainHt = FLTARR(!KON.Instr.NCAM,NumPts) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get terrain height for all points at the determined offset for each camera.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Isolate the coordinates at all points for this camera.
   ;------------------------------------------------------------------------

   temp_coords0 = REFORM(CamCoords[*,icam,*])

   ;------------------------------------------------------------------------
   ; Skip this camera to save time if all points are bad.
   ;------------------------------------------------------------------------

   ndxsgood = WHERE(temp_coords0 NE !KON.Misc.BADVALUE_REAL, numgood)
   IF (numgood EQ 0) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Loop over the across and along coordinates separately. Convert the
   ; 275 m coordinates to the coordinates appropriate for terrain data.
   ;------------------------------------------------------------------------

   FOR ixy=0,1 DO BEGIN

      temp_coords1 = REFORM(temp_coords0[ixy,*])

      ndxsgood = WHERE(temp_coords1 NE !KON.Misc.BADVALUE_REAL, numgood)
   
      IF (numgood GT 0) THEN BEGIN
         temp_coords2 = temp_coords1[ndxsgood]
         
         temp_coords2 = FLOOR(temp_coords2 / res_ratio[ixy])
         ndxsbad = WHERE(temp_coords2 LT 0, numbad)
         IF (numbad GT 0) THEN temp_coords2[ndxsbad] = 0
         ndxsbad = WHERE(temp_coords2 GT (sizes[ixy+1] - 1), numbad)
         IF (numbad GT 0) THEN temp_coords2[ndxsbad] = (sizes[ixy+1] - 1)

         temp_coords1[ndxsgood] = temp_coords2
      ENDIF

      temp_coords0[ixy,*] = temp_coords1

   ENDFOR

   ;-------------------------------------------------------------------------
   ; Only accept camera points if they have 2 good coordinates.
   ;-------------------------------------------------------------------------

   ndxsgood = WHERE(temp_coords0[0,*] GE 0 AND temp_coords0[1,*] GE 0 AND $
                    temp_coords0[0,*] LE (sizes[1] - 1) AND $
                    temp_coords0[1,*] LE (sizes[2] - 1), numgood)

   ;-------------------------------------------------------------------------
   ; Copy the correct terrain height values into the array to be returned.
   ; Interpolation isn't warranted here, because terrain heights are not
   ; smoothly changing and can't be accurately predicted.
   ;-------------------------------------------------------------------------

   TerrainHt[icam,ndxsgood] = $
        Terrain_Hts[temp_coords0[0,ndxsgood],temp_coords0[1,ndxsgood]] / 1000.0

ENDFOR

;---------------------------------------------------------------------------
; Clean up and return.
;---------------------------------------------------------------------------

temp_coords0 = 0
temp_coords1 = 0
temp_coords2 = 0
ndxsgood = 0
ndxsbad = 0

Retval = 0

END  ;  GetTerrainData

;***************************************************************************
PRO GetLandWaterData, State, PathNum, NumPts, MisrCoords, LandWaterMask, $
                      retval
;***************************************************************************
; Get the land/water mask data corresponding to the points passed in from
; the pre-loaded AGP buffer.
;---------------------------------------------------------------------------

COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; If the user's preferences file is damaged, the LandWaterMask takes a hit,
; so test for it and reload everything related here.
;---------------------------------------------------------------------------

result = ISA(Land_Water_Mask)
IF ~ ISA(Land_Water_Mask) THEN BEGIN
   !VAR.CurrFiles.AGP_Loaded = 0
   !VAR.CurrFiles.Biome_Loaded = 0
   Land_Water_Mask = 0
   Terrain_Hts = 0
   Biome_grid = 0
   Biome_swath = 0
   LoadAGPData, State, retval
   IF (retval NE 0) THEN RETURN
ENDIF

;---------------------------------------------------------------------------
; Get the size of the entire image at Land_Water_Mask resolution.
;---------------------------------------------------------------------------

sizes = SIZE(Land_Water_Mask)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP].Dims

cross_ratio = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims[0]
along_ratio = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution.  Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio) > 0) < (sizes[1] - 1)
wndw_crds[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio) > 0) < (sizes[2] - 1)

;---------------------------------------------------------------------------
; Copy the desired points to the return array.
;---------------------------------------------------------------------------

LandWaterMask = Land_Water_Mask[wndw_crds[0,*], wndw_crds[1,*]]

END  ;  GetLandWaterData

;***************************************************************************
PRO LoadBiomeData, State, Retval
;***************************************************************************
; Load MODIS MCD12C1 biome class type data corresponding to all the blocks
; that are currently loaded.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

;---------------------------------------------------------------------------
; Define biome grid constants.
;---------------------------------------------------------------------------

Biome_names = $
      ['Water', 'Evergreen Needleleaf forest', 'Evergreen Broadleaf forest', $
       'Deciduous Needleleaf forest', 'Deciduous Broadleaf forest', $
       'Mixed forest', 'Closed shrublands', 'Open shrublands', $
       'Woody savannas', 'Savannas', 'Grasslands', 'Permanent wetlands', $
       'Croplands', 'Urban and built-up', 'Cropland/Natural veg mosaic', $
       'Snow and ice', 'Barren or sparsely vegetated', 'Unclassified']
   
Biome_grid_spacing = 0.05  ; in degrees

;---------------------------------------------------------------------------
; If Biome grid is already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.Biome_Loaded) THEN RETURN

Retval = -1

num_lon = ROUND(360.0 / Biome_grid_spacing)
num_lat = ROUND(180.0 / Biome_grid_spacing)

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Get the name of the MODIS MCD12C1 file for the desired year.
;---------------------------------------------------------------------------

file_filter = ['*BiomeGrid*.dat']
temp_Biomefile = !VAR.CurrFiles.BiomeFile

GetLastFilename, 0, !KON.FileTyp.TypeBiomeClass, file_filter, 0, $
                 file_outpath, temp_Biomefile

!VAR.CurrFiles.BiomeFile = temp_Biomefile
IF (!VAR.CurrFiles.BiomeFile EQ '') THEN RETURN

IF (~ FILE_TEST(!VAR.CurrFiles.BiomeFile)) THEN BEGIN
   mssg = ['Biome file is not valid:', !VAR.CurrFiles.BiomeFile]
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   !VAR.CurrFiles.BiomeFile = ''
   !VAR.CurrFiles.Biome_Loaded = 0
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Read the binary file.
;---------------------------------------------------------------------------

Biome_grid = BYTARR(num_lon, num_lat)

GET_LUN, UnitF
OPENR, UnitF, temp_Biomefile
READU, UnitF, Biome_grid, TRANSFER_COUNT=num_elem
FREE_LUN, UnitF

siz_xy = SIZE(Biome_grid)
IF (siz_xy[1] NE num_lon OR siz_xy[2] NE num_lat) THEN BEGIN
   mssg = ['Array dimensions read = ' + STRTRIM(STRING(siz_xy[1]),2) + ' x ' + $
           STRTRIM(STRING(siz_xy[2]),2), 'Array dimensions should be = ' + $
           STRTRIM(STRING(num_lon),2) + ' x ' + STRTRIM(STRING(num_lat),2), $
           'Correct the biome grid file and try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   Biome_grid = 0
   !VAR.CurrFiles.BiomeFile = ''
   !VAR.CurrFiles.Biome_Loaded = 0
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Interpolate the biome grid at uniform 0.05 degrees spacing to a biome array
; for the entire loaded MISR swath.
;---------------------------------------------------------------------------

GetBiomeSwath, State, NumPts, LatLonCoords, Retval

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again and return.
;---------------------------------------------------------------------------

IF (Retval EQ 0) THEN !VAR.CurrFiles.Biome_Loaded = 1

END  ;  LoadBiomeData

;***************************************************************************
PRO GetBiomeSwath, State, NumPts, LatLonCoords, Retval
;***************************************************************************
; Get the biome classes corresponding to the lon/lat points passed in from
; the caller.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

Retval = 0

minx_biome_res = 1.100    ; desired biome pixel size in km
hires_per_biome_res = minx_biome_res / !KON.Instr.HI_RES_PIX_SIZE
                          ; number of MISR 275 m pixels per MINX biome pixel

;---------------------------------------------------------------------------
; Create arrays containing the window coords of the centers of pixels of
; size = minx_biome_res over the entire MISR swath currently loaded in MINX.
;---------------------------------------------------------------------------

numx = LONG(CoordStruct.(0).LRCwndwX / hires_per_biome_res)
numy = LONG(CoordStruct.(0).ULCwndwY / hires_per_biome_res)

wndw_x = LONARR(numx, numy)
wndw_y = LONARR(numx, numy)

num_pts = LONG(numx) * LONG(numy)
WndwCoord = LONARR(2, num_pts)

;---------------------------------------------------------------------------
; Get the window coordinates of the upper left corner pixel to act as the
; starting point for filling the coord array. Then fill the window coord
; array with coordinates for which we want biome classes.
;---------------------------------------------------------------------------

xy_init  = hires_per_biome_res / 2
xy_delta = hires_per_biome_res

xx = LINDGEN(numx) * xy_delta + xy_init
yy = LINDGEN(numy) * xy_delta + xy_init

FOR iy=0,numy-1 DO wndw_x[*,iy] = xx
WndwCoord[0,*] = REFORM(wndw_x, num_pts)

FOR ix=0,numx-1 DO wndw_y[ix,*] = yy
WndwCoord[1,*] = REFORM(wndw_y, num_pts)

xx = 0
yy = 0
wndw_x = 0
wndw_y = 0

;---------------------------------------------------------------------------
; Convert the window coordinates to lon/lat coordinates.
;---------------------------------------------------------------------------

WndwCrdToMisrCrd, State.Curframe, WndwCoord, MisrCoord, retval0
MisrCrdToSomCrd, State.Curframe, MisrCoord, SomCoord, retval1
SomCrdToLonlatCrd, State.Curframe, SomCoord, LatLonCoord, retval2

WndwCoord = 0
MisrCoord = 0
SomCoord = 0

IF (retval0 LT 0 OR retval1 LT 0 OR retval2 LT 0) THEN BEGIN
   Retval = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Convert lat/lon to array indices. Biome grid origin is at longitude = -180
; and latitude = +90, so reverse latitude indexes!
;---------------------------------------------------------------------------

num_lon = (SIZE(Biome_grid))[1]
num_lat = (SIZE(Biome_grid))[2]

lon_coeff = (360.0 - Biome_grid_spacing) / (360.0 * Biome_grid_spacing)
lat_coeff = (180.0 - Biome_grid_spacing) / (180.0 * Biome_grid_spacing) * (-1.0)

lon_ndxs = FLOOR(REFORM(LatLonCoord[0,*]) * lon_coeff + (num_lon - 1.0) / 2.0)
lat_ndxs = FLOOR(REFORM(LatLonCoord[1,*]) * lat_coeff + (num_lat - 1.0) / 2.0)

LatLonCoord = 0

;---------------------------------------------------------------------------
; Get the biome type code at each MINX biome pixel and resample to derive a
; biome map at 275 m resolution for overlaying on the MISR image. Also need
; to invert the y axis.
;--------------------------------------------------------------------------

Biome_swath = REFORM(Biome_grid[lon_ndxs, lat_ndxs])
lon_ndxs = 0
lat_ndxs = 0

Biome_swath = REBIN(REFORM(Biome_swath, numx, numy), $
                           numx * hires_per_biome_res, $
                           numy * hires_per_biome_res, /SAMPLE)
   
Biome_swath = REVERSE(Biome_swath, 2, /OVERWRITE)

END  ;  GetBiomeSwath

;***************************************************************************
PRO LoadGMPData, State, Retval
;***************************************************************************
; Load geometric parameter data corresponding to the GRP_ELLIPSOID data for
; all the blocks.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON blktimeangle, BlockCntrTime, SomToSwathAngles

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; If GEOM data are already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

DblBad = DOUBLE(!KON.Misc.BADVALUE_REAL)

IF (!VAR.CurrFiles.GMP_Loaded) THEN RETURN

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Get the filename for geometry data for this MISR path.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

file_filter = ['MISR*GP_GMP*' + orbit_str + '*.hdf']
temp_GMPfile = !VAR.CurrFiles.GMPfile
GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeGpGmp, $
                 file_filter, 0, file_outpath, temp_GMPfile

!VAR.CurrFiles.GMPfile = temp_GMPfile
IF (!VAR.CurrFiles.GMPfile EQ '') THEN BEGIN
   Retval = -2
   RETURN
ENDIF

IF (~ FILE_TEST(!VAR.CurrFiles.GMPfile)) THEN BEGIN
   mssg = ['GMP file not valid: ', !VAR.CurrFiles.GMPfile]
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Get the camera azimuth data.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
   IF (icam EQ 0) THEN OpenShut[0] = !KON.ProdTyp.OPEN_F
   IF (icam EQ !KON.Instr.NCAM-1) THEN OpenShut[1] = !KON.ProdTyp.SHUT_F

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_AZI', $
                icam, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for GP_GMP Camera azimuth angle.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup12
   ENDIF

   BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_AZI].BadValues[icam]
   ndxs = WHERE(databuf LE BadValues, numndxs)
   IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
   IF (icam EQ 0) THEN $
      Cam_Azimuth = DBLARR(9,(SIZE(databuf))[1], (SIZE(databuf))[2])

   Cam_Azimuth[icam,*,*] = databuf
ENDFOR

;---------------------------------------------------------------------------
; Get the camera zenith data.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
   IF (icam EQ 0) THEN OpenShut[0] = !KON.ProdTyp.OPEN_F
   IF (icam EQ !KON.Instr.NCAM-1) THEN OpenShut[1] = !KON.ProdTyp.SHUT_F

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_ZEN', $
                icam, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for GP_GMP Camera zenith angle.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup12
   ENDIF

   BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_ZEN].BadValues[icam]
   ndxs = WHERE(databuf LE BadValues, numndxs)
   IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
   IF (icam EQ 0) THEN $
      Cam_Zenith = DBLARR(9,(SIZE(databuf))[1], (SIZE(databuf))[2])

   Cam_Zenith[icam,*,*] = databuf
ENDFOR

;---------------------------------------------------------------------------
; Retrieve the average angle from swath north to SOM north for each camera
; of the loaded image.
;---------------------------------------------------------------------------

GetSomToSwathDirec, SomToSwathAngles, retval

;---------------------------------------------------------------------------
; Get the camera scatter data.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
   IF (icam EQ 0) THEN OpenShut[0] = !KON.ProdTyp.OPEN_F
   IF (icam EQ !KON.Instr.NCAM-1) THEN OpenShut[1] = !KON.ProdTyp.SHUT_F

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_SCT', $
                icam, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for GP_GMP Camera scatter angle.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup12
   ENDIF

   BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SCT].BadValues[icam]
   ndxs = WHERE(databuf LE BadValues, numndxs)
   IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
   IF (icam EQ 0) THEN $
      Cam_Scatter = DBLARR(9,(SIZE(databuf))[1], (SIZE(databuf))[2])

   Cam_Scatter[icam,*,*] = databuf
ENDFOR

;---------------------------------------------------------------------------
; Get the camera glitter data.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
   IF (icam EQ 0) THEN OpenShut[0] = !KON.ProdTyp.OPEN_F
   IF (icam EQ !KON.Instr.NCAM-1) THEN OpenShut[1] = !KON.ProdTyp.SHUT_F

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_GLT', $
                icam, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for GP_GMP Camera glitter angle.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup12
   ENDIF

   BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_GLT].BadValues[icam]
   ndxs = WHERE(databuf LE BadValues, numndxs)
   IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
   IF (icam EQ 0) THEN $
      Cam_Glitter = DBLARR(9,(SIZE(databuf))[1], (SIZE(databuf))[2])

   Cam_Glitter[icam,*,*] = databuf
ENDFOR
 
;---------------------------------------------------------------------------
; Get the sun azimuth data.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_SUN', $
             0, CoordStruct.(whichorbit).BlkBeg, $
             CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
             databuf, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for GP_GMP Sun azimuth angle.'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup12
ENDIF

BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SUN].BadValues[0]
ndxs = WHERE(databuf LE BadValues, numndxs)
IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
Sun_Azimuth = databuf

;---------------------------------------------------------------------------
; Get the sun zenith data.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.GMPfile, 'GPGMP_SUN', $
             1, CoordStruct.(whichorbit).BlkBeg, $
             CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
             databuf, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for GP_GMP Sun zenith angle.'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup12
ENDIF

BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SUN].BadValues[1]
ndxs = WHERE(databuf LE BadValues, numndxs)
IF (numndxs GT 0) THEN databuf[ndxs] = DblBad
 
Sun_Zenith = databuf

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again and clean up.
;---------------------------------------------------------------------------

!VAR.CurrFiles.GMP_Loaded = 1
ndxs = 0
databuf = 0
Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup12:
!VAR.CurrFiles.GMP_Loaded = 0
ndxs = 0
databuf = 0
Cam_Azimuth = 0
Cam_Zenith  = 0
Cam_Scatter = 0
Cam_Glitter = 0
Sun_Azimuth = 0
Sun_Zenith  = 0

END  ;  LoadGMPData

;***************************************************************************
PRO AdjustCamZenithForTerrain, State, Latitude, Terrain_Hts, Cam_Zenith, $
                               Retval
;***************************************************************************
; Apply a correction to all camera zenith angles                    x D 
; when GRP_TERRAIN data are being used so the                 . .  .
; angles are appropriate for the GRP_TERRAIN            .   .     .
; surface, not the GRP_ELLIPSOID surface.       A  x    .        .
;                                                B  x           .
;                                                    .         .
; D = satellite position                              .       .
; C = center of earth                                  .     .
; A = point on terrain                                  .   .
; B = point on ellipsoid                                 . .
;                                                         x C                                              
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

deg_to_rad = !DPI / 180.0
Retval = 0

;---------------------------------------------------------------------------
; Find the indices of all points with both valid zenith angles and valid
; terrain heights. Bad terrain hts occur for points for which a disparity
; was not successfully retrieved during matching. Only operate on good pts.
;---------------------------------------------------------------------------

ndxgood = WHERE(Cam_Zenith  NE !KON.Misc.BADVALUE_REAL AND $
                Terrain_Hts NE !KON.Misc.BADVALUE_REAL, numgood, $
                COMPLEMENT=ndxbad, NCOMPLEMENT=numbad)

zen = Cam_Zenith[ndxgood]
ter = Terrain_Hts[ndxgood]

;---------------------------------------------------------------------------
; Compute the local geocentric radius of the earth.
;---------------------------------------------------------------------------

local_earth_radius = ComputeGeocentricRadius(Latitude)

;---------------------------------------------------------------------------
; Compute the constant earth center to satellite distance.
;---------------------------------------------------------------------------

sideCD = local_earth_radius + !KON.Instr.MISR_altitude * 1000.0

;---------------------------------------------------------------------------
; Compute the earth center to terrain distance for camera at each point.
;---------------------------------------------------------------------------

sideCA = local_earth_radius + ter * 1000.0

;---------------------------------------------------------------------------
; Convert degrees to radians.
;---------------------------------------------------------------------------

zen *= deg_to_rad

;---------------------------------------------------------------------------
; Get the complement of the old zenith angles for this camera.
;---------------------------------------------------------------------------

angCBD = !DPI - zen

;---------------------------------------------------------------------------
; Get the angle at the center of the earth for the original zenith angle.
;---------------------------------------------------------------------------

angBCD = zen - ASIN(local_earth_radius / sideCD * SIN(angCBD))

;---------------------------------------------------------------------------
; Compute the length of the vector from the terrain to the satellite.
;---------------------------------------------------------------------------

sideAD = SQRT(sideCA^2 + sideCD^2 - 2.0 * sideCA * sideCD * COS(angBCD))

;---------------------------------------------------------------------------
; Compute the complement of the new zenith angle at the terrain.
;---------------------------------------------------------------------------

angCAD = ACOS((sideAD^2 + sideCA^2 - sideCD^2) / (2.0 * sideAD * sideCA))

;---------------------------------------------------------------------------
; Finally get the corrected zenith angle at the terrain. This is an
; approximation that assumes a flat earth!
;---------------------------------------------------------------------------

zen = !DPI - angCAD

IF (numgood GT 0) THEN Cam_Zenith[ndxgood] = zen / deg_to_rad
IF (numbad  GT 0) THEN Cam_Zenith[ndxbad]  = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

zen     = 0
ter     = 0
ndxgood = 0
ndxbad  = 0
angCBD  = 0
angBDC  = 0
angCAD  = 0
sideCA  = 0
sideAD  = 0

END  ;  AdjustCamZenithForTerrain

;***************************************************************************
PRO InterpolateGeomData, Zen1_Azi2, LoResGeomIn, FixAnZen, FixAzimuth, $
                         DebugPlt, GeomDims, ImageDims, HiResGeomOut, Retval
;***************************************************************************
; Convert the passed geometric data array from 17.6 km resolution to 275 m
; resolution using bilinear intrpolation. Certain geometry arrays require
; special handling. An camera zenith angles can't be linearly interpolated,
; because the shape of their functions on each row of data is a 'V' (only
; positive values are allowed). To adjust, change the sign of the first
; half of each row so the functions are monotonic.
; DebugPlt - set to 0 for production, 1 for hi-res, 2 for lo-res
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; If there is a minimum value in this row, find the pixel with the smallest
; value. Then to properly interpolate, invert the part of the row left of
; the minimum value to make the values monotonic. Parameter FixAnZen flags
; this. This is only needed for An camera zenith angle.
;---------------------------------------------------------------------------

IF (FixAnZen) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Loop over the rows. In each row, find the first and last geometry pixel
   ; that has valid data.
   ;------------------------------------------------------------------------

   FOR iline=0,GeomDims[1]-1 DO BEGIN      
      
      ndxs = WHERE(LoResGeomIn[*,iline] NE !KON.Misc.BADVALUE_REAL, numndxs)
      IF (numndxs LT 3) THEN CONTINUE
      
      beg_ndx = MIN(ndxs)
      end_ndx = MAX(ndxs)

      good_geom = REFORM(LoResGeomIn[beg_ndx:end_ndx, iline])
      mingeom = MIN(good_geom, minndx)
      
      IF (minndx NE 0 AND minndx NE N_ELEMENTS(good_geom)-1) THEN BEGIN
         IF (good_geom[minndx-1] LT good_geom[minndx+1]) THEN minndx -= 1
         good_geom[0:minndx] *= (-1.0)
         LoResGeomIn[beg_ndx:end_ndx, iline] = good_geom
      ENDIF
   
   ENDFOR

   good_geom = 0

ENDIF

;---------------------------------------------------------------------------
; The An camera and sometimes near-nadir cameras require special attention,
; when they switch polarity in the swath. Input resolution is inadequate to
; accurately interpolate here (Nyquist problem), so do the best we can.
;---------------------------------------------------------------------------

IF (FixAzimuth NE 0) THEN BEGIN

   ndxs = WHERE(LoResGeomIn NE !KON.Misc.BADVALUE_REAL, numndx)
   min_value = MIN(LoResGeomIn[ndxs], MAX=max_value)

   ;------------------------------------------------------------------------
   ; For non-nadir cameras, just temporarily adjust angles so they are not
   ; offset at the 0=360 point.
   ;------------------------------------------------------------------------

   IF (ABS(FixAzimuth) EQ 1 AND ABS(max_value-min_value) GT 320.D) THEN BEGIN
      ndxs = WHERE(LoResGeomIn GE 320.D, numndx)
      LoResGeomIn[ndxs] -= 360.D
      ndxs = 0
   ENDIF
   
ENDIF

;---------------------------------------------------------------------------
; Extrapolate the valid values to fill the invalid edge pixels.
;---------------------------------------------------------------------------

ndxs = WHERE(LoResGeomIn NE !KON.Misc.BADVALUE_REAL, numndxs)
xy_ndxs = ARRAY_INDICES(LoResGeomIn, ndxs)

TRIANGULATE, REFORM(xy_ndxs[0,*]), REFORM(xy_ndxs[1,*]), triangle, bound

x_out = FINDGEN(GeomDims[0])
y_out = FINDGEN(GeomDims[1])

new_grid = TRIGRID(REFORM(FLOAT(xy_ndxs[0,*])), REFORM(FLOAT(xy_ndxs[1,*])), $
                   LoResGeomIn[ndxs], triangle, EXTRAPOLATE=bound, $
                   XOUT=x_out, YOUT=y_out)
ndxs = 0
xy_ndxs = 0
triangle = 0
bound = 0
x_out = 0
y_out = 0

;---------------------------------------------------------------------------
; Do cubic interpolation using data at pixel centers to create a high
; resolution (275 m) grid of angles.
;---------------------------------------------------------------------------

IF (DebugPlt EQ 2) THEN BEGIN
   HiResGeomOut = REBIN(new_grid, ImageDims[0], ImageDims[1], /SAMPLE)
ENDIF ELSE BEGIN
   HiResGeomOut = CONGRID(new_grid, ImageDims[0], ImageDims[1], /CENTER, $
                          CUBIC=-0.5)
ENDELSE

new_grid = 0

;---------------------------------------------------------------------------
; Restore the azimuth angle array if changed above unless just for display.
; (If for display only, the value of FixAzimuth is negative.)
;---------------------------------------------------------------------------

IF (FixAzimuth NE 0) THEN BEGIN
   IF (FixAzimuth EQ 1 AND ABS(max_value-min_value) GT 320.D) THEN BEGIN
      ndxs = WHERE(HiResGeomOut LT 0.0, numndx)
      IF (numndx GT 0) THEN HiResGeomOut[ndxs] += 360.D
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Restore the first half of the An zenith array if previously inverted.
;---------------------------------------------------------------------------

IF (FixAnZen) THEN BEGIN
   ndxs = WHERE(HiResGeomOut NE !KON.Misc.BADVALUE_REAL AND $
                HiResGeomOut LT 0.0, numndxs)
   IF (numndxs GT 0) THEN HiResGeomOut[ndxs] *= (-1.0)
ENDIF

;---------------------------------------------------------------------------
; Make sure angles are within bounds (zenith 0 -> 90; azimuth 0 -> 360).
;---------------------------------------------------------------------------

IF (Zen1_Azi2 EQ 1) THEN BEGIN  ;  zenith angles
   ndxs = WHERE(HiResGeomOut GT 90.0, numndxs)
   IF (numndxs GT 0) THEN HiResGeomOut[ndxs] -= 90.0
   ndxs = WHERE(HiResGeomOut LT 0.0, numndxs)
   IF (numndxs GT 0) THEN HiResGeomOut[ndxs] += 90.0
ENDIF

IF (Zen1_Azi2 EQ 2) THEN BEGIN  ;  azimuth angles
   ndxs = WHERE(HiResGeomOut GT 360.0, numndxs)
   IF (numndxs GT 0) THEN HiResGeomOut[ndxs] -= 360.0
   ndxs = WHERE(HiResGeomOut LT 0.0, numndxs)
   IF (numndxs GT 0) THEN HiResGeomOut[ndxs] += 360.0
ENDIF

;---------------------------------------------------------------------------
; Clean up and return.
;---------------------------------------------------------------------------

ndxs = 0
Retval = 0

END  ;  InterpolateGeomData

;***************************************************************************
PRO ValidateAnGeomData, State, CamCoords, CamZenithAng, CamAzimuthAng
;***************************************************************************
; Visually validate interpolation. Turn off for production. Use this only
; with the "digitize along line" option.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Print the input values for all cameras.
;---------------------------------------------------------------------------

PRINT, ' Xcrd Ycrd    ZenAngle   AzimAngle'
FOR ival=0,N_ELEMENTS(CamCoords[0,*])-1 DO BEGIN
   PRINT, FORMAT='(2I5,2X,2F11.3)', $
          CamCoords[0,ival], CamCoords[1,ival], $
          CamZenithAng[ival], CamAzimuthAng[ival]
ENDFOR

;---------------------------------------------------------------------------
; Allocate arrays.
;---------------------------------------------------------------------------

Field = 'Camera Zenith (green) and Azimuth (red) angles for AN Camera'
SaveMisr = INTARR(3,3)
SaveDist = INTARR(3)
SaveAng1 = FLTARR(3)
SaveAng2 = FLTARR(3)

;---------------------------------------------------------------------------
; Select only valid input points.
;---------------------------------------------------------------------------

ndxsgood = WHERE(REFORM(CamCoords[0,*]) NE $
                 !KON.Misc.BADVALUE_REAL, num_pts)
IF (num_pts EQ 0) THEN RETURN

wndw_crds = REFORM(CamCoords[*,ndxsgood])

;---------------------------------------------------------------------------
; Get values of valid angles for this camera.
;---------------------------------------------------------------------------

Angle1 = REFORM(CamZenithAng[ndxsgood])
Angle2 = REFORM(CamAzimuthAng[ndxsgood])

;---------------------------------------------------------------------------
; Compute valid pixel distances from first point and convert window to MISR
; coordinates.
;---------------------------------------------------------------------------

Dists = DBLARR(num_pts)

FOR ipt=0,num_pts-1 DO BEGIN
   x_dist = wndw_crds[0,ipt] - wndw_crds[0,0]
   y_dist = wndw_crds[1,ipt] - wndw_crds[1,0]
   Dists[ipt] = SQRT(x_dist * x_dist + y_dist * y_dist)
ENDFOR
 
WndwCrdToMisrCrd, State.Curframe, wndw_crds, MisrCoords, Retval

;---------------------------------------------------------------------------
; Display the results.
;---------------------------------------------------------------------------

isavept = 0

FOR ipt=0,num_pts-1 DO BEGIN
   IF (ipt EQ num_pts/4 OR ipt EQ 2*num_pts/4 OR $
       ipt EQ 3*num_pts/4) THEN BEGIN
      SaveMisr[*,isavept] = MisrCoords[*,ipt]
      SaveDist[isavept] = Dists[ipt]
      SaveAng1[isavept] = Angle1[ipt]
      SaveAng2[isavept] = Angle2[ipt]
      isavept += 1
   ENDIF
ENDFOR

ndxs = WHERE(Angle1 GE 0.0 AND Angle2 GE 0.0, numndxs)
IF (numndxs EQ 0) THEN RETURN

Angle1 = Angle1[ndxs]
Angle2 = Angle2[ndxs]
distxy = Dists[ndxs]
MinAng = (MIN(Angle1) < MIN(Angle2)) * 0.94
MaxAng = (MAX(Angle1) > MAX(Angle2)) * 1.06
MaxDist = distxy[numndxs-1] - distxy[0]

WINDOW, 11, XPOS=800, YPOS=200, XSIZE=1600, YSIZE=1200, $
        TITLE='Test of Geometric Parameters'
PLOT, TITLE=Field, [0.0,0.01], [0.0,0.0], $
      MIN_VALUE=-5.0, XRANGE=[0,MaxDist], YRANGE=[MinAng,MaxAng], $
      XSTYLE=1, YSTYLE=1, XTICKLEN=1, YTICKLEN=1, COLOR=0, $
      XTITLE='Distance (pixels)', YTITLE='Angle (degrees)', $
      XTICKINTERVAL=50, YTICKINTERVAL=10, $
      XGRIDSTYLE=1, YGRIDSTYLE=1, BACKGROUND=16777215
OPLOT, distxy, Angle1, COLOR=65280, PSYM=-1
OPLOT, distxy, Angle2, COLOR=255, PSYM=-7
OPLOT, SaveDist, SaveAng1, COLOR=65280, PSYM=1
OPLOT, SaveDist, SaveAng2, COLOR=255, PSYM=7

FOR iout=0,2 DO BEGIN
  XYOUTS, SaveDist[iout], SaveAng1[iout]+1.0, STRING(FORMAT='(F7.3)', $
          SaveAng1[iout]), ALIGNMENT=0.5, COLOR=65280
  XYOUTS, SaveDist[iout], SaveAng2[iout]+1.0, STRING(FORMAT='(F7.3)', $
          SaveAng2[iout]), ALIGNMENT=0.5, COLOR=255
  misr_str = STRTRIM(STRING(SaveMisr[2,iout]),2) + '/' + $
             STRTRIM(STRING(SaveMisr[0,iout]),2) + '/' + $
             STRTRIM(STRING(SaveMisr[1,iout]),2)
  XYOUTS, SaveDist[iout], SaveAng1[iout]-3.0, misr_str, $
          ALIGNMENT=0.5, COLOR=65280
  misr_str = STRTRIM(STRING(SaveMisr[2,iout]),2) + '/' + $
             STRTRIM(STRING(SaveMisr[0,iout]),2) + '/' + $
             STRTRIM(STRING(SaveMisr[1,iout]),2)
  XYOUTS, SaveDist[iout], SaveAng2[iout]-3.0, misr_str, $
          ALIGNMENT=0.5, COLOR=255
ENDFOR

rtrn = DIALOG_MESSAGE('Press <OK> to continue', /CENTER, /INFO)

Dists = 0
cam_zen = 0
cam_azi = 0
wndw_crds = 0
SaveMisr = 0
SaveDist = 0
SaveAng1 = 0
SaveAng2 = 0
ndxsgood = 0
ndxs = 0

END  ;  ValidateAnGeomData

;***************************************************************************
PRO GetGeomData, GetParams, State, Latitude, NumPts, CamCoords, TerrainHts, $
                 CamAzimuthAng, CamZenithAng, CamScatterAng, $
                 CamGlitterAng, SunAzimuthAng, SunZenithAng, Retval
;***************************************************************************
; Get the camera geometric data corresponding to the points passed in from
; the pre-loaded GMP azimuth and zenith buffers. Points must be interpolated
; from 17.6 km pixels to 275 m pixels. NOTE - when GRP_TERRAIN data are being
; used, a correction must be made to adjust camera zenith angles from the
; ellipsoid surface to the terrain surface. 
; GetParams specifies which geometric parameters are needed:
;   1 = only retrieve Cam_Zenith (not used)
;   2 = only retrieve Cam_Zenith and Cam_Azimuth
;   3 = retrieve all parameters
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith

COMPILE_OPT IDL2, LOGICAL_PREDICATE

DEBUG_PLT = 0  ;  set to 0 for production,
               ;   1 for hi-res display test, 2 for lo-res
Retval = -1

;---------------------------------------------------------------------------
; Allocate arrays to return.
;---------------------------------------------------------------------------

DblBad = DOUBLE(!KON.Misc.BADVALUE_REAL)

CamZenithAng  = DBLARR(!KON.Instr.NCAM,NumPts) + DblBad
IF (GetParams GT 1) THEN BEGIN
   CamAzimuthAng = DBLARR(!KON.Instr.NCAM,NumPts) + DblBad
   SunZenithAng  = DBLARR(NumPts) + DblBad
   IF (GetParams EQ 3) THEN BEGIN
      CamScatterAng = DBLARR(!KON.Instr.NCAM,NumPts) + DblBad
      CamGlitterAng = DBLARR(!KON.Instr.NCAM,NumPts) + DblBad
      SunAzimuthAng = DBLARR(NumPts) + DblBad
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Get the dimensions of lo-res geometry data and hi-res image data for the
; entire image in "assembled" window coordinates.
;---------------------------------------------------------------------------

dims_scene_image = [CoordStruct.(0).LRCwndwX, CoordStruct.(0).ULCwndwY]
dims_scene_geom  = (SIZE(Sun_Azimuth))[1:2]

;---------------------------------------------------------------------------
; Get geometry for all points at the determined offset for each camera.
;---------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Isolate the coordinates at all points for this camera.
   ;------------------------------------------------------------------------

   wndw_crds = REFORM(CamCoords[*,icam,*])

   ;------------------------------------------------------------------------
   ; Skip this camera to save time if all points are bad.
   ;------------------------------------------------------------------------

   ndxsgood = WHERE(wndw_crds NE !KON.Misc.BADVALUE_REAL, numgood)
   IF (numgood EQ 0) THEN CONTINUE

   ;-------------------------------------------------------------------------
   ; Only accept camera points if they have 1+ good coordinates.
   ;-------------------------------------------------------------------------

   ndxsgood = WHERE(wndw_crds[0,*] GE 0.0 AND wndw_crds[1,*] GE 0.0 AND $
                    wndw_crds[0,*] LE (dims_scene_image[0] - 1) AND $
                    wndw_crds[1,*] LE (dims_scene_image[1] - 1), numgood)
   ndxsgood = 0  
                    
   IF (numgood EQ 0) THEN CONTINUE
   
   ;-------------------------------------------------------------------------
   ; Interpolate the lo-res camera zenith data to hi-res. Then loop over the
   ; input sample points and find their corresponding camera zenith values.
   ; All camera zenith angle functions have a minimum across the swath, but
   ; only the An camera requires special attention. So before interpolation,
   ; temporarily force each row to be monotonic by inverting half the row's
   ; values (icam+1 in procedure call).
   ;-------------------------------------------------------------------------

   camera_zenith = REFORM(Cam_Zenith[icam,*,*])
   fix_zenith = (icam EQ !KON.Instr.AN) ? 1 : 0

   InterpolateGeomData, 1, camera_zenith, fix_zenith, 0, DEBUG_PLT, $
                        dims_scene_geom, dims_scene_image, resamp_cam_zen, $
                        status
                   
   FOR ipt=0,NumPts-1 DO BEGIN

      IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
          wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

      CamZenithAng[icam,ipt] = resamp_cam_zen[ROUND(wndw_crds[0,ipt]), $
                                              ROUND(wndw_crds[1,ipt])]
   ENDFOR
                        
   camera_zenith = 0
   resamp_cam_zen = 0

   ;------------------------------------------------------------------------
   ; If GRP_TERRAIN data are loaded, we need to correct camera zenith angles
   ; for the fact that zenith angles stored in GP_GMP files are appropriate
   ; for GRP_ELLIPSOID data only. The correction is a function of the zenith
   ; angle and the terrain height at each point. The correction is small.
   ;------------------------------------------------------------------------

   IF (STRPOS(!VAR.CurrFiles.CamFiles[4], 'TERRAIN') GE 0) THEN BEGIN   
      IF (N_ELEMENTS(TerrainHts) NE 1 OR TerrainHts[0] NE 0.0) THEN BEGIN
      
         terrain_hts = REFORM(TerrainHts[icam,*])
         ndxter = WHERE(terrain_hts GT -999.0, numter)
         IF (numter EQ 0) THEN BEGIN
            CamZenithAng[icam,*] = !KON.Misc.BADVALUE_REAL
         ENDIF ELSE BEGIN
            cam_zen_ang = REFORM(CamZenithAng[icam,*])
            AdjustCamZenithForTerrain, State, Latitude, terrain_hts, cam_zen_ang, $
                                       Retval
            CamZenithAng[icam,*] = cam_zen_ang
            cam_zen_ang = 0
         ENDELSE
         terrain_hts = 0
         ndxter = 0

      ENDIF      
   ENDIF

   ;---------------------------------------------------------------------
   ; We only need Cam_Zenith to determine the SOM to swath direction.
   ;---------------------------------------------------------------------

   IF (GetParams EQ 1) THEN CONTINUE

   ;-------------------------------------------------------------------------
   ; Interpolate the lo-res camera azimuth data to hi-res. Then loop over the
   ; input sample points and find their corresponding camera azimuth values.
   ; The An camera requires special attention (not yet implemented).
   ;-------------------------------------------------------------------------

   camera_azimuth = REFORM(Cam_Azimuth[icam,*,*])

   azim_cam = (icam EQ !KON.Instr.AN) ? 2 : 1

   InterpolateGeomData, 2, camera_azimuth, 0, azim_cam, DEBUG_PLT, dims_scene_geom, $
                        dims_scene_image, resamp_cam_azim, status
                   
   FOR ipt=0,NumPts-1 DO BEGIN

      IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
          wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

      temp_angle = resamp_cam_azim[ROUND(wndw_crds[0,ipt]), $
                                   ROUND(wndw_crds[1,ipt])]

      ;---------------------------------------------------------------------
      ; The GP_GMP product standard is for the azimuth angle to point from
      ; the ground toward the instrument. This program's standard is the
      ; opposite, so we must change values by 180 degrees while keeping them
      ; between 0 and 360. 
      ;---------------------------------------------------------------------

      IF (temp_angle NE !KON.Misc.BADVALUE_REAL) THEN temp_angle -= 180.0D
      IF (temp_angle LT 0.0D) THEN temp_angle += 360.0D
      CamAzimuthAng[icam,ipt] = temp_angle
      
   ENDFOR
        
   camera_azimuth = 0
   resamp_cam_azim = 0

   ;---------------------------------------------------------------------
   ; If desired, plot the successful camera zenith and azimuth angles for
   ; the An camera in either hi-res (after interpolation) of lo-res
   ; (before interpolation).
   ;---------------------------------------------------------------------

   IF (DEBUG_PLT GT 0 AND icam EQ !KON.Instr.AN) THEN BEGIN
      ValidateAnGeomData, State, REFORM(CamCoords[*,!KON.Instr.AN,*]), $
                          REFORM(CamZenithAng[!KON.Instr.AN,*]), $
                          REFORM(CamAzimuthAng[!KON.Instr.AN,*])
   ENDIF

   ;---------------------------------------------------------------------
   ; Get the solar zenith angle at each point. It's not necessary to
   ; interpolate, since it is used only for broadband albedo.
   ;---------------------------------------------------------------------

   IF (icam EQ !KON.Instr.AN) THEN BEGIN
      
      solar_zenith = Sun_Zenith

      InterpolateGeomData, 1, solar_zenith, 0, 0, 0, dims_scene_geom, $
                           dims_scene_image, resamp_sun_zen, status
                   
      FOR ipt=0,NumPts-1 DO BEGIN

         IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
             wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

         SunZenithAng[ipt] = resamp_sun_zen[ROUND(wndw_crds[0,ipt]), $
                                            ROUND(wndw_crds[1,ipt])]
      ENDFOR

      solar_zenith = 0
      resamp_sun_zen = 0

   ENDIF

   ;------------------------------------------------------------------------
   ; We only need Cam_Zenith, Cam_Azimuth and Sun_Zenith for ht retrievals.
   ; (Sun_Zenith is needed only if computing albedos.)
   ;------------------------------------------------------------------------
      
   IF (GetParams EQ 2) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Interpolate the lo-res camera scatter angle data to hi-res. Then loop
   ; over the input sample points and find their corresponding camera
   ; scatter angle values.
   ;------------------------------------------------------------------------

   camera_scatter = REFORM(Cam_Scatter[icam,*,*])

   InterpolateGeomData, 3, camera_scatter, 0, 0, 0, dims_scene_geom, $
                        dims_scene_image, resamp_cam_scat, status
                   
   FOR ipt=0,NumPts-1 DO BEGIN

      IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
          wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

      CamScatterAng[icam,ipt] = resamp_cam_scat[ROUND(wndw_crds[0,ipt]), $
                                                ROUND(wndw_crds[1,ipt])]
   ENDFOR
            
   camera_scatter = 0
   resamp_cam_scat = 0
                        
   ;------------------------------------------------------------------------
   ; Interpolate the lo-res camera glitter angle data to hi-res. Then loop
   ; over the input sample points and find their corresponding camera
   ; glitter angle values.
   ;------------------------------------------------------------------------

   camera_glitter = REFORM(Cam_Glitter[icam,*,*])

   InterpolateGeomData, 3, camera_glitter, 0, 0, DEBUG_PLT, dims_scene_geom, $
                        dims_scene_image, resamp_cam_glit, status
                   
   FOR ipt=0,NumPts-1 DO BEGIN

      IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
          wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

      CamGlitterAng[icam,ipt] = resamp_cam_glit[ROUND(wndw_crds[0,ipt]), $
                                                ROUND(wndw_crds[1,ipt])]
   ENDFOR

   camera_glitter = 0
   resamp_cam_glit = 0

   ;------------------------------------------------------------------------
   ; Interpolate the lo-res sun azimuth angle data to hi-res. Then loop
   ; over the input sample points and find their corresponding sun azimuth
   ; angle values.
   ;------------------------------------------------------------------------

   IF (icam EQ !KON.Instr.AN) THEN BEGIN

      solar_azimuth = Sun_Azimuth

      InterpolateGeomData, 2, solar_azimuth, 0, 0, DEBUG_PLT, dims_scene_geom, $
                           dims_scene_image, resamp_sun_azim, status
                   
      FOR ipt=0,NumPts-1 DO BEGIN

         IF (wndw_crds[0,ipt] EQ !KON.Misc.BADVALUE_REAL OR $
             wndw_crds[1,ipt] EQ !KON.Misc.BADVALUE_REAL) THEN CONTINUE

         SunAzimuthAng[ipt] = resamp_sun_azim[ROUND(wndw_crds[0,ipt]), $
                                              ROUND(wndw_crds[1,ipt])]
      ENDFOR

      solar_azimuth = 0
      resamp_sun_azim = 0

   ENDIF

ENDFOR
   
;---------------------------------------------------------------------------
; Clean up and return.
;---------------------------------------------------------------------------

dims_scene_geom = 0
dims_scene_image = 0
wndw_crds = 0
ndxs = 0

Retval = 0

END  ;  GetGeomData

;***************************************************************************
PRO GetSomToSwathDirec, SomToSwathAngles, Retval
;---------------------------------------------------------------------------
; Because swath angles differ for each camera, we need to define the swath
; angle for all cameras. Use across-track pixel locations for the first and
; last valid pixels on each line of the swath and fit linear functions to
; them (i.e. on both sides of the swath). Use only the red band. The
; resulting swath angles are measured in a clockwise direction from SOM
; north (positive values are clockwise).
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

SomToSwathAngles = FLTARR(!KON.Instr.NCAM) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Define the swath angles using red band data.
;---------------------------------------------------------------------------

num_line = CoordStruct.(0).NumBlk * !KON.Instr.HI_RES_PIX_ALONG
line_num = INTARR(num_line,2)
first_valid = INTARR(num_line,2)

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
   pImage = !VAR.RawImages[!KON.Instr.RED, icam]
   ngood_line0 = 0
   ngood_line1 = 0
   
   FOR ilin=0,num_line-1 DO BEGIN
      ndxs = WHERE((*pImage)[*,ilin] GT 0.0, numndxs)

      IF (numndxs GT 0) THEN BEGIN
         minndx = MIN(ndxs, MAX=maxndx)
         IF (minndx GT 0) THEN BEGIN
            line_num[ngood_line0,0] = ilin
            first_valid[ngood_line0,0] = minndx
            ngood_line0 += 1
         ENDIF
         IF (maxndx GT 0) THEN BEGIN
            line_num[ngood_line1,1] = ilin
            first_valid[ngood_line1,1] = maxndx
            ngood_line1 += 1
         ENDIF
      ENDIF
   ENDFOR

   ;------------------------------------------------------------------------
   ; Perform a linear regression fit to the first valid pixels and use the
   ; slope to compute the angle between the SOM along direction and the
   ; swath ground track direction. Reverse the sign so a clockwise rotation
   ; from SOM north to swath north is positive. Do this on both sides of the
   ; swath and average them.
   ;------------------------------------------------------------------------

   IF (MIN(first_valid[*,0]) EQ MAX(first_valid[*,0])) THEN BEGIN
      angle0 = 0.0
      isOK0 = 0 
   ENDIF ELSE BEGIN
      bcoeff = REGRESS(line_num[0:ngood_line0-1,0], $
                       first_valid[0:ngood_line0-1,0], $
                       CONST=acoeff, CORRELATION=corr, STATUS=isOK0)
      IF (isOK0 EQ 0) THEN angle0 = ATAN(bcoeff[0]) * (-1.0)
   ENDELSE

   IF (MIN(first_valid[*,1]) EQ MAX(first_valid[*,1])) THEN BEGIN
      angle1 = 0.0
      isOK1 = 0 
   ENDIF ELSE BEGIN
      bcoeff = REGRESS(line_num[0:ngood_line1-1,1], $
                       first_valid[0:ngood_line1-1,1], $
                       CONST=acoeff, CORRELATION=corr, STATUS=isOK1)
      IF (isOK1 EQ 0) THEN angle1 = ATAN(bcoeff[0]) * (-1.0)
   ENDELSE

   IF (isOK0 EQ 0 AND isOK1 EQ 0) THEN $
      SomToSwathAngles[icam] = (angle0 + angle1) / 2.0
ENDFOR

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

ndxs = 0
line_num = 0
first_valid = 0

Retval = 0

END  ;  GetSomToSwathDirec

;***************************************************************************
PRO GetPgeAerosolData, State, CoordStruct, NumPts, MisrCoords, AerTau_BE, $
                       AerTau_LR, AerAngexp_BE, AerAngexp_LR, AerSsa, $
                       AerTauFrac, Retval
;***************************************************************************
; Load aerosol data fields corresponding to all blocks currently loaded.
; All fields used are floats and all have -9999.0 as bad value fill.
;---------------------------------------------------------------------------

COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

AerTau_BE = 0
AerTau_LR = 0
AerSsa = 0
AerAngexp_BE = 0
AerAngexp_LR = 0
AerTauFrac = 0

;---------------------------------------------------------------------------
; If aerosol data are already loaded, don't do it again.
;---------------------------------------------------------------------------

IF (~ !VAR.CurrFiles.AE1_Loaded) THEN BEGIN

   whichorbit = (State.curframe GT 9)

   ;------------------------------------------------------------------------
   ; Set the orbit number string.
   ;------------------------------------------------------------------------

   orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

   ;------------------------------------------------------------------------
   ; Get the filename for aerosol data for this MISR path.
   ;------------------------------------------------------------------------

   file_filter = ['MISR*AEROSOL*' + orbit_str + '*.hdf']
   temp_AE1file = !VAR.CurrFiles.AE1file
   GetLastFilename, CoordStruct.(whichorbit).PathNum, $
                    !KON.FileTyp.TypeAerosol, file_filter, 0, $
                    file_outpath, temp_AE1file
   !VAR.CurrFiles.AE1file = temp_AE1file
   IF (!VAR.CurrFiles.AE1file EQ '') THEN BEGIN
      Retval = -2
      RETURN
   ENDIF

   IF (~ FILE_TEST(!VAR.CurrFiles.AE1file)) THEN RETURN

   ;------------------------------------------------------------------------
   ; Set hourglass cursor.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, /HOURGLASS

   ;------------------------------------------------------------------------
   ; Loop over the bands for TAU best estimate field. Allocate arrays the
   ; first time they are used after we know their size. All fields are in
   ; the same grid, so open file and grid at beginning and close at end.
   ;------------------------------------------------------------------------

   OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]

   Tau_BE_Grid = 0
   databuf = 0

   FOR iband=0,3 DO BEGIN

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_TAU_BE', $
                   iband, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (iband EQ 0) THEN $
         OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for Aerosol best estimate optical depth.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup14
      ENDIF
 
      IF (iband EQ 0) THEN $
         Tau_BE_Grid = FLTARR(4,(SIZE(databuf))[1], $
                                (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
      Tau_BE_Grid[iband,*,*] = databuf

   ENDFOR

   ;------------------------------------------------------------------------
   ; Loop over the bands for SSA field. Allocate arrays the first time they
   ; are are used after we know their size.
   ;------------------------------------------------------------------------

   Ssa_Grid = 0
   databuf = 0

   FOR iband=0,3 DO BEGIN

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_SSA', $
                   iband, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for Aerosol single scatter albedo.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup14
      ENDIF
 
      IF (iband EQ 0) THEN $
         Ssa_Grid = FLTARR(4,(SIZE(databuf))[1], $
                             (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
      Ssa_Grid[iband,*,*] = databuf

   ENDFOR

   ;------------------------------------------------------------------------
   ; Loop over the bands for TAU fraction field. Allocate arrays the first
   ; time they are used after we know their size. Only data for the green
   ; band are loaded. The 5 particle types are [small, medium, large,
   ; spherical, non-spherical]
   ;------------------------------------------------------------------------

   Taufrac_Grid = 0
   databuf = 0

   FOR ipart=0,4 DO BEGIN

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_FRC', $
                   ipart, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for Aerosol optical depth fraction.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup14
      ENDIF
 
      IF (ipart EQ 0) THEN $
         Taufrac_Grid = FLTARR(5,(SIZE(databuf))[1], $
                                 (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
      Taufrac_Grid[ipart,*,*] = databuf

   ENDFOR

   ;------------------------------------------------------------------------
   ; Get data for best estimate angstrom exponent field.
   ;------------------------------------------------------------------------

   Angexp_BE_Grid = 0
   databuf = 0

   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_AEX_BE', $
                0, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for Aerosol best estimate angstrom exponent.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup14
   ENDIF
 
   Angexp_BE_Grid = databuf

   ;------------------------------------------------------------------------
   ; Loop over the bands for TAU lowest residual field. Allocate arrays the
   ; first time they are used after we know their size.
   ;------------------------------------------------------------------------

   OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]

   Tau_LR_Grid = 0
   databuf = 0

   FOR iband=0,3 DO BEGIN

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_TAU_LR', $
                   iband, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (iband EQ 0) THEN $
         OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for Aerosol lowest residual optical depth.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup14
      ENDIF
 
      IF (iband EQ 0) THEN $
         Tau_LR_Grid = FLTARR(4,(SIZE(databuf))[1], $
                                (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
      Tau_LR_Grid[iband,*,*] = databuf

   ENDFOR

   ;------------------------------------------------------------------------
   ; Get data for lowest residual angstrom exponent field.
   ;------------------------------------------------------------------------

   Angexp_LR_Grid = 0
   databuf = 0

   OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.AE1file, 'AS_AEX_LR', $
                0, CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for Aerosol lowest residual angstrom exponent.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup14
   ENDIF
 
   Angexp_LR_Grid = databuf

ENDIF

;---------------------------------------------------------------------------
; Return if user didn't ask to get values at specific points.
;---------------------------------------------------------------------------

!VAR.CurrFiles.AE1_Loaded = 1
databuf = 0
Retval = 0
IF (NumPts EQ 0) THEN RETURN

;---------------------------------------------------------------------------
; Allocate arrays. Caller must free them when done. This is currently done
; only for best estimate parameters.
;---------------------------------------------------------------------------

Retval = -1

AerTau_BE    = FLTARR(4,NumPts) + !KON.Misc.BADVALUE_REAL
AerSsa       = FLTARR(4,NumPts) + !KON.Misc.BADVALUE_REAL
AerTauFrac   = FLTARR(5,NumPts) + !KON.Misc.BADVALUE_REAL
AerAngExp_BE = FLTARR(NumPts)   + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the size of the aerosol images at their resolution.
;---------------------------------------------------------------------------

sizes = SIZE(Angexp_BE_Grid)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_TAU_BE].Dims

cross_ratio = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims[0]
along_ratio = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution. Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio) > 0) < (sizes[1] - 1)
wndw_crds[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio) > 0) < (sizes[2] - 1)

;---------------------------------------------------------------------------
; Loop over the points on the profile.
;---------------------------------------------------------------------------

FOR ipt=0,NumPts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Copy the desired points to the return array.
   ;------------------------------------------------------------------------

   FOR iband=0,3 DO BEGIN
      AerTau_BE[iband,ipt] = Tau_BE_Grid[iband,wndw_crds[0,ipt],wndw_crds[1,ipt]]
      AerSsa[iband,ipt] = Ssa_Grid[iband,wndw_crds[0,ipt],wndw_crds[1,ipt]]
   ENDFOR

   FOR ipart=0,4 DO BEGIN
      AerTauFrac[ipart,ipt] = Taufrac_Grid[ipart,wndw_crds[0,ipt],wndw_crds[1,ipt]]
   ENDFOR

   AerAngExp_BE[ipt] = Angexp_BE_Grid[wndw_crds[0,ipt],wndw_crds[1,ipt]]

ENDFOR

sizes = 0
dims = 0
wndw_crds = 0
Retval = 0

RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup14:
!VAR.CurrFiles.AE1_Loaded = 0
sizes = 0
dims = 0
wndw_crds = 0
Tau_BE_Grid = 0
Tau_LR_Grid = 0
Ssa_Grid = 0
Angexp_BE_Grid = 0
Angexp_LR_Grid = 0
Taufrac_Grid = 0

END  ;  GetPgeAerosolData

;***************************************************************************
PRO GetPgeLandData, State, CoordStruct, NumPts, MisrCoords, BHRdata, $
                    DHRdata, NDVIdata, RPVdata, LandFile, Retval
;***************************************************************************
; Load AS_LAND data for the requested blocks.
; NOTE - Land data need to be unpacked and scaled. They are also byte types,
;        so bad values are positive integers.
;---------------------------------------------------------------------------

COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

BHRdata  = 0
DHRdata  = 0
NDVIdata = 0
RPVdata  = 0

;---------------------------------------------------------------------------
; If land data are already loaded, don't do it again.
;---------------------------------------------------------------------------

IF (~ !VAR.CurrFiles.LND_Loaded) THEN BEGIN

   whichorbit = (State.curframe GT 9)

   ;------------------------------------------------------------------------
   ; Set the orbit number string.
   ;------------------------------------------------------------------------

   orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

   ;------------------------------------------------------------------------
   ; Get the filename for land data for this MISR path.
   ;------------------------------------------------------------------------

   file_filter = ['MISR*AS_LAND*' + orbit_str + '*.hdf']
   GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeLand, $
                    file_filter, 0, file_outpath, LandFile
   !VAR.CurrFiles.LNDfile = LandFile
   IF (!VAR.CurrFiles.LNDfile EQ '') THEN BEGIN
      Retval = -2
      RETURN
   ENDIF

   IF (~ FILE_TEST(!VAR.CurrFiles.LNDfile)) THEN RETURN

   ;------------------------------------------------------------------------
   ; Set hourglass cursor.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, /HOURGLASS

   ;------------------------------------------------------------------------
   ; Get the offset and scaling parameters for Land variables.
   ;------------------------------------------------------------------------

   file_id = EOS_GD_OPEN(!VAR.CurrFiles.LNDfile)
   grid_id = EOS_GD_ATTACH(file_id, 'SubregParamsLnd')

   status  = EOS_GD_READATTR(grid_id, 'Scale LandBHR',       ScaleBHR)
   status  = EOS_GD_READATTR(grid_id, 'Offset LandBHR',      OffsetBHR)
   status  = EOS_GD_READATTR(grid_id, 'Scale LandDHR',       ScaleDHR)
   status  = EOS_GD_READATTR(grid_id, 'Offset LandDHR',      OffsetDHR)
   status  = EOS_GD_READATTR(grid_id, 'Scale NDVI',          ScaleNDVI)
   status  = EOS_GD_READATTR(grid_id, 'Offset NDVI',         OffsetNDVI)
   status  = EOS_GD_READATTR(grid_id, 'Scale BRFModParam1',  ScaleRPV1)
   status  = EOS_GD_READATTR(grid_id, 'Offset BRFModParam1', OffsetRPV1)
   status  = EOS_GD_READATTR(grid_id, 'Scale BRFModParam2',  ScaleRPV2)
   status  = EOS_GD_READATTR(grid_id, 'Offset BRFModParam2', OffsetRPV2)
   status  = EOS_GD_READATTR(grid_id, 'Scale BRFModParam3',  ScaleRPV3)
   status  = EOS_GD_READATTR(grid_id, 'Offset BRFModParam3', OffsetRPV3)

   status  = EOS_GD_DETACH(grid_id)
   status  = EOS_GD_CLOSE(file_id)

   ;------------------------------------------------------------------------
   ; Load the BHR data for 4 bands. Unpack and scale the data.
   ;------------------------------------------------------------------------

   OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]

   BHR_Grid = 0

   FOR iband=0,3 DO BEGIN
      databuf = 0
      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.LNDfile, 'AS_LAND1', $
                   iband, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for AS_LAND BHR data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup18
      ENDIF
 
      IF (iband EQ 0) THEN BEGIN
         OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
         BHR_Grid = FLTARR(4,(SIZE(databuf))[1], $
                             (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
      ENDIF

      databuf = FLOAT(databuf)
      BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND1].BadValues[iband]
      ndxs = WHERE(databuf LT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                                  NCOMPLEMENT=numbad)
      IF (numndxs GT 0) THEN $
         databuf[ndxs] = (databuf[ndxs] * ScaleBHR[0]) + OffsetBHR[0]
      IF (numbad GT 0) THEN $
         databuf[ndxsbad] = !KON.Misc.BADVALUE_REAL

      BHR_Grid[iband,*,*] = databuf
   ENDFOR

   ;------------------------------------------------------------------------
   ; Load the DHR data for 4 bands. Unpack and scale the data.
   ;------------------------------------------------------------------------

   DHR_Grid = 0

   FOR iband=0,3 DO BEGIN
      databuf = 0
      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.LNDfile, 'AS_LAND1', $
                   iband+4, CoordStruct.(whichorbit).BlkBeg, $
                   CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                   databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for AS_LAND DHR data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup18
      ENDIF
 
      IF (iband EQ 0) THEN $
         DHR_Grid = FLTARR(4,(SIZE(databuf))[1], $
                             (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL

      databuf = FLOAT(databuf)
      BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND1].BadValues[iband+4]
      ndxs = WHERE(databuf LT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                                  NCOMPLEMENT=numbad)
      IF (numndxs GT 0) THEN $
         databuf[ndxs] = (databuf[ndxs] * ScaleDHR[0]) + OffsetDHR[0]
      IF (numbad GT 0) THEN $
         databuf[ndxsbad] = !KON.Misc.BADVALUE_REAL

      DHR_Grid[iband,*,*] = databuf
   ENDFOR

   ;------------------------------------------------------------------------
   ; Load the NDVI data for the green band. Unpack and scale the data.
   ;------------------------------------------------------------------------

   NDVI_Grid = 0
   databuf = 0

   LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.LNDfile, 'AS_LAND2', 0, $
                CoordStruct.(whichorbit).BlkBeg, $
                CoordStruct.(whichorbit).BlkEnd, file_id, grid_id, $
                databuf, status

   IF (status NE 0) THEN BEGIN
      mssg = 'LoadHdfData failed for AS_LAND NDVI data.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      GOTO, cleanup18
   ENDIF
 
   databuf = FLOAT(databuf)
   BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND2].BadValues[0]
   ndxs = WHERE(databuf LT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                               NCOMPLEMENT=numbad)
   IF (numndxs GT 0) THEN $
      databuf[ndxs] = (databuf[ndxs] * ScaleNDVI[0]) + OffsetNDVI[0]
   IF (numbad GT 0) THEN $
      databuf[ndxsbad] = !KON.Misc.BADVALUE_REAL

   NDVI_Grid = databuf

   ;------------------------------------------------------------------------
   ; Load the RPV parameters for the green band. Unpack and scale the data.
   ;------------------------------------------------------------------------

   RPV_Grid = 0

   FOR iparm=0,2 DO BEGIN
      databuf = 0
      IF (iparm EQ 2) THEN OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.LNDfile, 'AS_LAND2', iparm+1, $
                   CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
                   file_id, grid_id, databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for AS_LAND RPV parameter.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup18
      ENDIF

      IF (iparm EQ 0) THEN $
         RPV_Grid = FLTARR(3,(SIZE(databuf))[1], $
                             (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL

      scalefctr = (iparm EQ 0) ? ScaleRPV1[0]  : $
                  (iparm EQ 1) ? ScaleRPV2[0]  : ScaleRPV3[0]
      offstfctr = (iparm EQ 0) ? OffsetRPV1[0] : $
                  (iparm EQ 1) ? OffsetRPV2[0] : OffsetRPV3[0]

      databuf = FLOAT(databuf)
      BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND2].BadValues[iparm+1]
      ndxs = WHERE(databuf LT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                                  NCOMPLEMENT=numbad)

      IF (numndxs GT 0) THEN $
         databuf[ndxs] = (databuf[ndxs] * scalefctr) + offstfctr
      IF (numbad GT 0) THEN $
         databuf[ndxsbad] = !KON.Misc.BADVALUE_REAL

      RPV_Grid[iparm,*,*] = databuf
   ENDFOR

ENDIF

!VAR.CurrFiles.LND_Loaded = 1
databuf = 0
ndxs = 0
ndxsbad = 0
Retval = 0
RETURN

cleanup18:
!VAR.CurrFiles.LND_Loaded = 0
sizes = 0
dims = 0
ndxs = 0
databuf = 0
wndw_crds = 0
BHR_Grid = 0
DHR_Grid = 0
NDVI_Grid = 0
RPV_Grid = 0

END  ;  GetPgeLandData

;***************************************************************************
PRO LoadTCStereoData, State, StereoFile, Retval
;***************************************************************************
; Load PGE stereo height data for the points on this feature. TC_STEREO wind
; data present a special problem: the resolution is 70.4 km while the 
; horizontal offset of blocks is 17.6 km. So we must increase the resolution
; of wind data by x4 to make this work.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File

;---------------------------------------------------------------------------
; If TC_STEREO data are already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.STR_Loaded) THEN RETURN

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set the orbit number string.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

;---------------------------------------------------------------------------
; Get the filename for PGE product for this orbit.
;---------------------------------------------------------------------------

file_filter = ['MISR*TC_STEREO*' + orbit_str + '*.hdf']
GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeStereo, $
                 file_filter, 0, file_outpath, Stereo_File

StereoFile = Stereo_File

IF (Stereo_File EQ '' OR ~FILE_TEST(Stereo_File)) THEN BEGIN
   Retval = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Read the data in this region for TC_STEREO 1.1 km data.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_11', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_ZeroHts, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO zero wind height'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF
STER_ZeroHts = FLOAT(STER_ZeroHts)

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_11', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_CorrHts, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO wind-corrected height'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF
STER_CorrHts = FLOAT(STER_CorrHts)

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_11', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_SDCM, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO cloud mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF
STER_SDCM = FLOAT(STER_SDCM)

;---------------------------------------------------------------------------
; Read the data in this region for TC_STEREO 70.4 km data.
; Data are already floats with bad value flags of -9999.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_704', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_WndCrossLo, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO EW low winds'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_704', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_WndAlongLo, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO NS low winds'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_704', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_WndCrossHi, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO EW high winds'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, Stereo_File, 'STER_704', 3, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, STER_WndAlongHi, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_STEREO NS high winds'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup15
ENDIF

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again.
;---------------------------------------------------------------------------

!VAR.CurrFiles.STR_Loaded = 1
Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup15:
!VAR.CurrFiles.STR_Loaded = 0
STER_ZeroHts = 0
STER_CorrHts = 0
STER_WndCrossLo = 0
STER_WndAlongLo = 0
STER_WndCrossHi = 0
STER_WndAlongHi = 0
STER_SDCM = 0

END ;  LoadTCStereoData

;***************************************************************************
PRO GetPgeStereoData, State, NumPts, MisrCoords, ZeroWindHts, CorrWindHts, $
                      EwWind, NsWind, StereoFile, Retval
;***************************************************************************
; Load PGE stereo height data for the points on this feature.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
Retval = 0

StereoFile = Stereo_File

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Free arrays then reallocate them.  Caller must free them when done.
;---------------------------------------------------------------------------

ZeroWindHts = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
CorrWindHts = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
EwWind      = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
NsWind      = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the size of the stereo height and wind grids.
;---------------------------------------------------------------------------

sizes_hts  = SIZE(STER_ZeroHts)
sizes_wnds = SIZE(STER_WndCrossLo)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims_hts = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_11].Dims
cross_ratio_hts = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_hts[0]
along_ratio_hts = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_hts[1]

dims_wnds = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_704].Dims
cross_ratio_wnds = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_wnds[0]
along_ratio_wnds = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_wnds[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution.  Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds_hts  = INTARR(2,NumPts)
wndw_crds_wnds = INTARR(2,NumPts)

wndw_crds_hts[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_hts) > 0) < $
                      (sizes_hts[1] - 1)
wndw_crds_hts[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_hts) > 0) < $
                      (sizes_hts[2] - 1)

wndw_crds_wnds[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_wnds) > 0) < $
                      (sizes_wnds[1] - 1)
wndw_crds_wnds[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_wnds) > 0) < $
                      (sizes_wnds[2] - 1)

;---------------------------------------------------------------------------
; Loop over the points on the profile and get the values from the stereo
; file for those coordinates.
;---------------------------------------------------------------------------

FOR ipt=0,NumPts-1 DO BEGIN
   ZeroWindHts[ipt] = STER_ZeroHts[wndw_crds_hts[0,ipt], wndw_crds_hts[1,ipt]]
   CorrWindHts[ipt] = STER_CorrHts[wndw_crds_hts[0,ipt], wndw_crds_hts[1,ipt]]

   cross_wnd1 = STER_WndCrossLo[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
   cross_wnd2 = STER_WndCrossHi[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
   IF (cross_wnd1 NE !KON.Misc.BADVALUE_REAL) THEN BEGIN
      EwWind[ipt] = cross_wnd1
   ENDIF ELSE BEGIN
      EwWind[ipt] = cross_wnd2
   ENDELSE

   along_wnd1 = STER_WndAlongLo[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
   along_wnd2 = STER_WndAlongHi[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
   IF (along_wnd1 NE !KON.Misc.BADVALUE_REAL) THEN BEGIN
      NsWind[ipt] = along_wnd1
   ENDIF ELSE BEGIN
      NsWind[ipt] = along_wnd2
   ENDELSE
ENDFOR

;---------------------------------------------------------------------------
; Convert heights from meters to km.
;---------------------------------------------------------------------------

ndxs = WHERE(ZeroWindHts GT !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs GT 0) THEN ZeroWindHts[ndxs] /= 1000.0

ndxs = WHERE(CorrWindHts GT !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs GT 0) THEN CorrWindHts[ndxs] /= 1000.0

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

sizes_hts = 0
sizes_wnds = 0
dims_hts = 0
dims_wnds = 0
wndw_crds = 0
wndw_crds_hts = 0
wndw_crds_wnds = 0

END  ;  GetPgeStereoData

;***************************************************************************
PRO LoadTCCloudData, State, CloudFile, Retval
;***************************************************************************
; Load PGE stereo height data for the points on this feature.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON coord_data, CoordStruct
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File

;---------------------------------------------------------------------------
; If TC_CLOUD data are already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.CLD_Loaded) THEN RETURN

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set the orbit number string.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

;---------------------------------------------------------------------------
; Get the filename for PGE product for this orbit.
;---------------------------------------------------------------------------

file_filter = ['MISR*TC_CLOUD*' + orbit_str + '*.hdf']
GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeCloud, $
                 file_filter, 0, file_outpath, Cloud_File

CloudFile = Cloud_File

IF (Cloud_File EQ '' OR ~FILE_TEST(Cloud_File)) THEN BEGIN
   Retval = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Read the data in this region for TC_CLOUD 1.1 km no-wind data.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDZ_11', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDZ_ZeroHts, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD zero-wind height'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDZ_ZeroHts = FLOAT(CLDZ_ZeroHts)

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDZ_11', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDZ_WndCross, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD zero-wind cross-track wind'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDZ_WndCross = FLOAT(CLDZ_WndCross)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDZ_11].BadValues[1]
ndxs = WHERE(CLDZ_WndCross GT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                                  NCOMPLEMENT=numbad)
IF (numndxs GT 0) THEN CLDZ_WndCross[ndxs] *= 0.01
IF (numbad  GT 0) THEN CLDZ_WndCross[ndxsbad] = !KON.Misc.BADVALUE_REAL

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDZ_11', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDZ_CldMsk, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD zero-wind cloud mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDZ_CldMsk = FLOAT(CLDZ_CldMsk)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDZ_11].BadValues[2]
ndxs = WHERE(CLDZ_CldMsk LE BadValues, numndxs)
IF (numndxs GT 0) THEN CLDZ_CldMsk[ndxs] = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Read the data in this region for TC_CLOUD 1.1 km with-wind data.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDC_11', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDC_CorrHts, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD wind-corrected height'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDC_CorrHts = FLOAT(CLDC_CorrHts)

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDC_11', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDC_WndCross, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD corrected cross-track wind'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDC_WndCross = FLOAT(CLDC_WndCross)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDC_11].BadValues[1]
ndxs = WHERE(CLDC_WndCross GT BadValues, numndxs, COMPLEMENT=ndxsbad, $
                                                  NCOMPLEMENT=numbad)
IF (numndxs GT 0) THEN CLDC_WndCross[ndxs] *= 0.01
IF (numbad  GT 0) THEN CLDC_WndCross[ndxsbad] = !KON.Misc.BADVALUE_REAL

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLDC_11', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLDC_SDCM, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD wind-corrected cloud mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF
CLDC_SDCM = FLOAT(CLDC_SDCM)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDC_11].BadValues[2]
ndxs = WHERE(CLDC_SDCM LE BadValues, numndxs)
IF (numndxs GT 0) THEN CLDC_SDCM[ndxs] = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Read the data in this region for TC_CLOUD 17.6 km data.
; Winds are already floats with bad value flag of -9999.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLD_176', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLD_MotionHts, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD 17.6 motion heights'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLD_176', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLD_WndAlong, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD 17.6 along-track wind'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLD_176', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLD_WndCross, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD 17.6 cross-track wind'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]
LoadHdfData, whichorbit, OpenShut, 1, Cloud_File, 'CLD_176', 3, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, CLD_CldMsk, status
IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for TC_CLOUD 17.6 cloud mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup16
ENDIF

CLD_CldMsk = FLOAT(CLD_CldMsk)
BadValues = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLD_176].BadValues[3]
ndxs = WHERE(CLD_CldMsk LE BadValues, numndxs)
IF (numndxs GT 0) THEN CLD_CldMsk[ndxs] = !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again.
;---------------------------------------------------------------------------

!VAR.CurrFiles.CLD_Loaded = 1
ndxs = 0
Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup16:
!VAR.CurrFiles.CLD_Loaded = 0
ndxs = 0
CLDZ_ZeroHts = 0
CLDC_CorrHts = 0
CLD_MotionHts = 0
CLDZ_WndCross = 0
CLDC_WndCross = 0
CLD_WndCross = 0
CLD_WndAlong = 0
CLDZ_CldMsk = 0
CLDC_SDCM = 0
CLD_CldMsk = 0

END  ;  LoadTCCloudData

;***************************************************************************
PRO GetPgeCloudData, State, NumPts, MisrCoords, ZeroWindHts, $
                     CorrWindHts, EwWind, NsWind, CloudFile, Retval
;***************************************************************************
; Load PGE stereo height data for the points on this feature.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
Retval = 0

CloudFile = Cloud_File

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Free arrays then reallocate them.  Caller must free them when done.
;---------------------------------------------------------------------------

ZeroWindHts = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
CorrWindHts = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
EwWind      = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL
NsWind      = FLTARR(NumPts) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the size of the stereo height grids.
;---------------------------------------------------------------------------

sizes_hts  = SIZE(CLDZ_ZeroHts)
sizes_wnds = SIZE(CLDC_WndCross)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims_hts = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDZ_11].Dims
cross_ratio_hts = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_hts[0]
along_ratio_hts = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_hts[1]

dims_wnds = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLD_176].Dims
cross_ratio_wnds = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_wnds[0]
along_ratio_wnds = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_wnds[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution.  Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds_hts  = INTARR(2,NumPts)
wndw_crds_wnds = INTARR(2,NumPts)

wndw_crds_hts[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_hts) > 0) < $
                      (sizes_hts[1] - 1)
wndw_crds_hts[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_hts) > 0) < $
                      (sizes_hts[2] - 1)

wndw_crds_wnds[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_wnds) > 0) < $
                      (sizes_wnds[1] - 1)
wndw_crds_wnds[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_wnds) > 0) < $
                      (sizes_wnds[2] - 1)

;---------------------------------------------------------------------------
; Loop over the points on the profile and get the values from the stereo
; file for those coordinates.
;---------------------------------------------------------------------------

FOR ipt=0,NumPts-1 DO BEGIN
   ZeroWindHts[ipt] = CLDZ_ZeroHts[wndw_crds_hts[0,ipt], wndw_crds_hts[1,ipt]]
   CorrWindHts[ipt] = CLDC_CorrHts[wndw_crds_hts[0,ipt], wndw_crds_hts[1,ipt]]
   EwWind[ipt] = CLD_WndCross[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
   NsWind[ipt] = CLD_WndAlong[wndw_crds_wnds[0,ipt], wndw_crds_wnds[1,ipt]]
ENDFOR

;---------------------------------------------------------------------------
; Convert heights from meters to km.
;---------------------------------------------------------------------------

ndxs = WHERE(ZeroWindHts GT !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs GT 0) THEN ZeroWindHts[ndxs] /= 1000.0

ndxs = WHERE(CorrWindHts GT !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs GT 0) THEN CorrWindHts[ndxs] /= 1000.0

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

sizes_hts = 0
sizes_wnds = 0
dims_hts = 0
dims_wnds = 0
wndw_crds = 0
wndw_crds_hts = 0
wndw_crds_wnds = 0

END  ;  GetPgeCloudData

;***************************************************************************
PRO LoadSVMData, State, Retval
;***************************************************************************
; Load SVM smoke mask data corresponding to all blocks currently loaded.
; NOTE - These are the only product fields that are not converted to FLOAT.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; If SVM data are already loaded, don't do it again.
;---------------------------------------------------------------------------

Retval = 0

IF (!VAR.CurrFiles.SVM_Loaded) THEN RETURN

Retval = -1
whichorbit = (State.curframe GT 9)

;---------------------------------------------------------------------------
; Set hourglass cursor.
;---------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS

;---------------------------------------------------------------------------
; Get the filename for SVM data for this MISR path.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

file_filter = ['MISR*CLASSIFIERS*' + orbit_str + '*.hdf']
GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeClass, file_filter, $
                 0, file_outpath, newSVMfile
!VAR.CurrFiles.SVMfile = newSVMfile
IF (~ FILE_TEST(!VAR.CurrFiles.SVMfile)) THEN RETURN

;---------------------------------------------------------------------------
; Read the data in this region.
;---------------------------------------------------------------------------

OpenShut = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.SVMfile, 'TC_SVM', 0, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Cloud_Confid, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for SVM Cloud_Confid mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup13
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.SVMfile, 'TC_SVM', 1, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Smoke_Confid, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for SVM Smoke_Confid mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup13
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.SVMfile, 'TC_SVM', 2, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Dust_Confid, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for SVM Dust_Confid mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup13
ENDIF

OpenShut = [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F]

LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.SVMfile, 'TC_SVM', 3, $
             CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
             file_id, grid_id, Land_Confid, status

IF (status NE 0) THEN BEGIN
   mssg = 'LoadHdfData failed for SVM Land_Confid mask'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   GOTO, cleanup13
ENDIF

;---------------------------------------------------------------------------
; Create masks for smoke and land at 275m scale. NO - disable.
;---------------------------------------------------------------------------

;Cloud_Confid = CONGRID(Cloud_Confid, State.sizex, State.sizey, /CENTER)
;Smoke_Confid = CONGRID(Smoke_Confid, State.sizex, State.sizey, /CENTER)
;Dust_Confid  = CONGRID(Dust_Confid,  State.sizex, State.sizey, /CENTER)
;Land_Confid  = CONGRID(Land_Confid,  State.sizex, State.sizey, /CENTER)

;---------------------------------------------------------------------------
; Set the flag so this doesn't get loaded again.
;---------------------------------------------------------------------------

!VAR.CurrFiles.SVM_Loaded = 1
Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup13:
!VAR.CurrFiles.SVM_Loaded = 0
Cloud_Confid = 0
Smoke_Confid = 0
Dust_Confid = 0
Land_Confid = 0
SVM_Mapinfo = 0

END  ;  LoadSVMData

;***************************************************************************
PRO GetSvmData, State, PathNum, NumPts, MisrCoords, Offsets, SmokeMask, $
                DustMask, LandMask, Retval
;***************************************************************************
; Get the smoke and land confidence level data corresponding to the points
; passed in from the pre-loaded SVM buffers. THS ROUTINE IS NO LONGER USED.
;---------------------------------------------------------------------------

COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;---------------------------------------------------------------------------
; Get the size of the entire image at mask resolution.
;---------------------------------------------------------------------------

sizes = SIZE(Cloud_Confid)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TC_SVM].Dims

cross_ratio = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims[0]
along_ratio = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution.  Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio) > 0) < (sizes[1] - 1)
wndw_crds[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio) > 0) < (sizes[2] - 1)

;---------------------------------------------------------------------------
; Copy the desired points to the return array.
;---------------------------------------------------------------------------

CloudMask = Cloud_Confid[wndw_crds[0,*], wndw_crds[1,*]]
SmokeMask = Smoke_Confid[wndw_crds[0,*], wndw_crds[1,*]]
DustMask  = Dust_Confid [wndw_crds[0,*], wndw_crds[1,*]]
LandMask  = Land_Confid [wndw_crds[0,*], wndw_crds[1,*]]

Retval = 0

END  ;  GetSvmData

;***************************************************************************
PRO GetPgeAlbedoData, State, CoordStruct, NumPts, MisrCoords, LocalAlbedo, $
                      RestrAlbedo, ExpansAlbedo, AlbedoFile, Retval
;***************************************************************************
; Load PGE8C albedo data for the requested blocks.
; All albedo parameters used are FLOAT with bad value values of -9999.
;---------------------------------------------------------------------------

COMMON alb_data, Local_Albedo, Expans_Albedo, Restrict_Albedo

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

LocalAlbedo  = 0
RestrAlbedo  = 0
ExpansAlbedo = 0

;---------------------------------------------------------------------------
; If albedo data are already loaded, don't do it again.
;---------------------------------------------------------------------------

IF (~ !VAR.CurrFiles.ALB_Loaded) THEN BEGIN

   whichorbit = (State.curframe GT 9)

   ;------------------------------------------------------------------------
   ; Set the orbit number string.
   ;------------------------------------------------------------------------

   orbit_str = STRTRIM(STRING(CoordStruct.(whichorbit).OrbitNum),2)

   ;------------------------------------------------------------------------
   ; Get the filename for albedo data for this MISR path.
   ;------------------------------------------------------------------------

   file_filter = ['MISR*ALBEDO*' + orbit_str + '*.hdf']
   GetLastFilename, CoordStruct.(whichorbit).PathNum, !KON.FileTyp.TypeAlbedo, $
                    file_filter, 0, file_outpath, AlbedoFile
   !VAR.CurrFiles.ALBfile = AlbedoFile
   IF (!VAR.CurrFiles.ALBfile EQ '') THEN BEGIN
      Retval = -2
      RETURN
   ENDIF

   IF (~ FILE_TEST(!VAR.CurrFiles.ALBfile)) THEN RETURN

   ;------------------------------------------------------------------------
   ; Set hourglass cursor.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, /HOURGLASS

   ;------------------------------------------------------------------------
   ; Loop over the bands for local albedo field. Allocate arrays the first
   ; time they are used after we know their size. These fields are in the
   ; same grid, so open file and grid at beginning and close at end.
   ;------------------------------------------------------------------------

   Local_Albedo = 0
   databuf = 0

   FOR iband=0,4 DO BEGIN
      OpenShut = (iband EQ 0) ? [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F] : $
                 (iband EQ 4) ? [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F] : $
                                [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.ALBfile, 'ALB_22', iband, $
                   CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
                   file_id, grid_id, databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for local albedo data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         IF (iband EQ 0) THEN Local_Albedo = FLTARR(5,(SIZE(databuf))[1], $
                                    (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
         Local_Albedo[iband,*,*] = databuf
      ENDELSE
   ENDFOR

   ;------------------------------------------------------------------------
   ; Loop over the bands for restrictive albedo field.
   ;------------------------------------------------------------------------

   Restrict_Albedo = 0
   databuf = 0

   FOR iband=0,4 DO BEGIN
      OpenShut = (iband EQ 0) ? [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F] : $
                 (iband EQ 4) ? [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F] : $
                                [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.ALBfile, 'ALB_352a', iband, $
                   CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
                   file_id, grid_id, databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for restrictive albedo data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         IF (iband EQ 0) THEN Restrict_Albedo = FLTARR(5,(SIZE(databuf))[1], $
                                 (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
         Restrict_Albedo[iband,*,*] = databuf
      ENDELSE
   ENDFOR

   ;------------------------------------------------------------------------
   ; Loop over the bands for expansive albedo field.
   ;------------------------------------------------------------------------

   Expans_Albedo = 0
   databuf = 0

   FOR iband=0,4 DO BEGIN
      OpenShut = (iband EQ 0) ? [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.KEEP_F] : $
                 (iband EQ 4) ? [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.SHUT_F] : $
                                [!KON.ProdTyp.KEEP_F, !KON.ProdTyp.KEEP_F]

      LoadHdfData, whichorbit, OpenShut, 1, !VAR.CurrFiles.ALBfile, 'ALB_352b', iband, $
                   CoordStruct.(whichorbit).BlkBeg, CoordStruct.(whichorbit).BlkEnd, $
                   file_id, grid_id, databuf, status

      IF (status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed for expansive albedo data.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         GOTO, cleanup17
      ENDIF ELSE BEGIN
         IF (iband EQ 0) THEN Expans_Albedo = FLTARR(5,(SIZE(databuf))[1], $
                                 (SIZE(databuf))[2]) + !KON.Misc.BADVALUE_REAL
         Expans_Albedo[iband,*,*] = databuf
      ENDELSE
   ENDFOR

ENDIF

;---------------------------------------------------------------------------
; Return if user didn't ask to get values at specific points.
;---------------------------------------------------------------------------

!VAR.CurrFiles.ALB_Loaded = 1
databuf = 0
Retval = 0
IF (NumPts EQ 0) THEN RETURN

;---------------------------------------------------------------------------
; Allocate arrays. Caller must free them when done.
;---------------------------------------------------------------------------

Retval = -1

LocalAlbedo  = FLTARR(5,NumPts) + !KON.Misc.BADVALUE_REAL
RestrAlbedo  = FLTARR(5,NumPts) + !KON.Misc.BADVALUE_REAL
ExpansAlbedo = FLTARR(5,NumPts) + !KON.Misc.BADVALUE_REAL

;---------------------------------------------------------------------------
; Get the size of the aerosol images at their resolution.
;---------------------------------------------------------------------------

sizes_L = SIZE(Local_Albedo)
sizes_R = SIZE(Restrict_Albedo)

;---------------------------------------------------------------------------
; Get the resolution coversion factors.
;---------------------------------------------------------------------------

dims_L = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_22].Dims
cross_ratio_L = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_L[0]
along_ratio_L = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_L[1]

dims_R = !KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_352a].Dims
cross_ratio_R = FLOAT(!KON.Instr.HI_RES_PIX_CROSS) / dims_R[0]
along_ratio_R = FLOAT(!KON.Instr.HI_RES_PIX_ALONG) / dims_R[1]

;---------------------------------------------------------------------------
; Convert points from block, line, sample to window coords, then convert
; coordinates to the correct resolution.  Window coordinates are the same as
; "assembled" block coordinates except along-track coordinates are inverted
; (bottom-to-top vs top-to-bottom), so don't do it again in MisrCrdToWndwCrd
; (Invert = 0).
;---------------------------------------------------------------------------

MisrCrdToWndwCrd, State.Curframe, MisrCoords, wndw_crds, 0, Retval

wndw_crds_L[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_L) > 0) < (sizes_L[1] - 1)
wndw_crds_L[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_L) > 0) < (sizes_L[2] - 1)

wndw_crds_R[0,*] = (FLOOR(wndw_crds[0,*] / cross_ratio_R) > 0) < (sizes_R[1] - 1)
wndw_crds_R[1,*] = (FLOOR(wndw_crds[1,*] / along_ratio_R) > 0) < (sizes_R[2] - 1)

;---------------------------------------------------------------------------
; Loop over the points on the profile.
;---------------------------------------------------------------------------

FOR ipt=0,NumPts-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Copy the desired points to the return array.
   ;------------------------------------------------------------------------

   FOR iband=0,4 DO BEGIN
      LocalAlbedo [iband,ipt] = $
                  Local_Albedo   [iband,wndw_crds_L[0,ipt],wndw_crds_L[1,ipt]]
      RestrAlbedo [iband,ipt] = $
                  Restrict_Albedo[iband,wndw_crds_R[0,ipt],wndw_crds_R[1,ipt]]
      ExpansAlbedo[iband,ipt] = $
                  Expans_Albedo  [iband,wndw_crds_R[0,ipt],wndw_crds_R[1,ipt]]
   ENDFOR

ENDFOR

;---------------------------------------------------------------------------
; Clean up after normal load.
;---------------------------------------------------------------------------

Retval = 0
RETURN

;---------------------------------------------------------------------------
; Clean up the data on failure.
;---------------------------------------------------------------------------

cleanup17:
!VAR.CurrFiles.ALB_Loaded = 0
LocalAlbedo  = 0
RestrAlbedo  = 0
ExpansAlbedo = 0

END  ;  GetPgeAlbedoData

;****************************************************************************
PRO LoadHdfData, Whichorbit, OpenShut, OffsetBlks, FileName, ProductName, $
                 Ifield, BlkBeg, BlkEnd, FileId, GridId, DataAry, Status
;****************************************************************************
; Read the requested data field from a MISR standard product and offset it to
; assemble the blocks into a smooth swath if requested.
;----------------------------------------------------------------------------

COMMON coord_data, CoordStruct

Status   = -1
DO_DEBUG =  0

;----------------------------------------------------------------------------
; Make sure the file contains the block range needed.
;----------------------------------------------------------------------------

GetFirstLastBlocks, Filename, begBlk, endBlk, Retval

IF (Retval NE 0) THEN RETURN

IF (begBlk GT BlkBeg OR endBlk LT BlkEnd) THEN BEGIN
   mssg = ['File does not contain all the MISR blocks requested.', $
           'Correct the problem before trying again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Find the requested product type and field.
;----------------------------------------------------------------------------

found = 0

FOR iprod=0,!KON.ProdTyp.MaxGrids-1 DO BEGIN
   IF (ProductName EQ !KON.ProdTyp.ProdParms[iprod].ProductName) THEN BEGIN
      ProdStruct = !KON.ProdTyp.ProdParms[iprod]
      found = 1
      BREAK
   ENDIF
ENDFOR

IF (~found) THEN BEGIN
   mssg = 'Could not find product name: ' + ProductName
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Update the block begin and end values.
;----------------------------------------------------------------------------

ProdStruct.Dims[2]   = BlkEnd - BlkBeg + 1
ProdStruct.Starts[2] = BlkBeg - 1

;----------------------------------------------------------------------------
; Set an error handler.
;----------------------------------------------------------------------------

CATCH, iErr
IF (iErr NE 0) THEN BEGIN
   mssg = ['There is a problem reading data from file:', FileName, $
           'Fix the problem and try running again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   CATCH, /CANCEL
   RETURN
ENDIF

;----------------------------------------------------------------------------
; If OpenShut is 1 or 3, open the file and the grid where the data field is
; located. Otherwise the file and grid must still be open for reading another
; field. If OpenShut is 2 or 3, close the file and the grid after use. If 
; OpenShut is 0, neither open or close the file - it is already open and
; stays open for reuse.
;----------------------------------------------------------------------------

IF (OpenShut[0] EQ !KON.ProdTyp.OPEN_F) THEN BEGIN

   FileId = EOS_GD_OPEN(FileName, /READ) 

   IF (FileId LT 0) THEN BEGIN
      mssg = ['Error attempting to open file:', FileName]
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      CATCH, /CANCEL
      RETURN
   ENDIF

   GridId = EOS_GD_ATTACH(FileId, ProdStruct.GridName)

   IF (GridId LT 0) THEN BEGIN
      mssg = ['Error attempting to open grid: ', ProdStruct.GridName]
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      CATCH, /CANCEL
      RETURN
   ENDIF

ENDIF

;----------------------------------------------------------------------------
; If extra dimension(s) are tacked onto the field's basename, isolate the
; basename and the dimension(s).
;----------------------------------------------------------------------------

FieldName = ProdStruct.FieldNames[Ifield]

nPos1a = STRPOS(FieldName, '[')
nPos2b = -1
IF (nPos1a GT 0) THEN BEGIN
   nPos1b = STRPOS(FieldName, ']')
   DimNum1 = FIX(STRMID(FieldName, nPos1a+1, nPos1b-nPos1a))

   nPos2a = STRPOS(FieldName, '[', /REVERSE_SEARCH)
   IF (nPos2a GT 0 AND nPos2a NE nPos1a) THEN BEGIN
      nPos2b = STRPOS(FieldName, ']', /REVERSE_SEARCH)
      DimNum2 = FIX(STRMID(FieldName, nPos2a+1, nPos2b-nPos2a))
   ENDIF

   FieldName = STRMID(FieldName, 0, nPos1a)
ENDIF

;----------------------------------------------------------------------------
; If the field has multiple dimensions, prefix the edge, start, stride.
;----------------------------------------------------------------------------

edges   = ProdStruct.Dims
starts  = ProdStruct.Starts
strides = [1, 1, 1]

IF (nPos1a GT 0) THEN BEGIN
   edges   = [1, edges]
   starts  = [DimNum1, starts]
   strides = [1, strides]
ENDIF

IF (nPos2b GT 0) THEN BEGIN
   edges   = [1, edges]
   starts  = [DimNum2, starts]
   strides = [1, strides]
ENDIF

;----------------------------------------------------------------------------
; If reading a CMM stereo file, change the starting block number so it is
; a count relative to the first block in the file. For MISR production data,
; this is the absolute block number (relative to 1).
;----------------------------------------------------------------------------

IF (ProductName EQ 'STER_11' OR ProductName EQ 'STER_704') THEN BEGIN
   starts = [0,0,4]
ENDIF

;----------------------------------------------------------------------------
; Read all blocks of the data in one call for this data field.
;----------------------------------------------------------------------------

Status = EOS_GD_READFIELD(GridId, FieldName, DataAry, $
                          EDGE=edges, START=starts, STRIDE=strides) 
starts  = 0
strides = 0

IF (Status NE 0) THEN BEGIN
   mssg = ['Error attempting to read data from file/grid/field:', $
           'File : ' + FileName, 'Grid : ' + ProdStruct.GridName, $
           'Field: ' + FieldName]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   RETURN
ENDIF

DataAry = REFORM(DataAry)

;----------------------------------------------------------------------------
; If this is TC_STEREO wind data, we must increase the resolution by x4 so
; the native 70.4 km pixels are not larger than the 17.6 km block offsets.
; If this is TC_ALBEDO expansive or restrictive data, we must increase the
; resolution by x2 so the native 35.2 km pixels are not larger than the 17.6
; km block offsets.
;----------------------------------------------------------------------------

IF (ProductName EQ 'STER_704') THEN BEGIN
   sizes = SIZE(DataAry)
   IF (sizes[0] EQ 2) THEN BEGIN
      DataAry = REBIN(DataAry, sizes[1]*4, sizes[2]*4, /SAMPLE)
   ENDIF ELSE BEGIN
      DataAry = REBIN(DataAry, sizes[1]*4, sizes[2]*4, sizes[3], /SAMPLE)
   ENDELSE
ENDIF

IF (ProductName EQ 'ALB_352a' OR ProductName EQ 'ALB_352b') THEN BEGIN
   sizes = SIZE(DataAry)
   IF (sizes[0] EQ 2) THEN BEGIN
      DataAry = REBIN(DataAry, sizes[1]*2, sizes[2]*2, /SAMPLE)
   ENDIF ELSE BEGIN
      DataAry = REBIN(DataAry, sizes[1]*2, sizes[2]*2, sizes[3], /SAMPLE)
   ENDELSE
ENDIF

;----------------------------------------------------------------------------
; If done reading fields, close the grid and the file. Cancel error handler.
;----------------------------------------------------------------------------

IF (OpenShut[1] EQ !KON.ProdTyp.SHUT_F) THEN BEGIN
   Status = EOS_GD_DETACH(GridId) 
   Status = EOS_GD_CLOSE(FileId) 
ENDIF

CATCH, /CANCEL

;----------------------------------------------------------------------------
; Get the value which acts as the threshold below which (if negative) or
; above which (if positive) the value is not valid.
;----------------------------------------------------------------------------

BadValue = ProdStruct.BadValues[Ifield]

;----------------------------------------------------------------------------
; Concatenate the blocks to eliminate the block dimension.
;----------------------------------------------------------------------------

ary_size  = SIZE(DataAry)
numBlk    = ((ary_size)[0] EQ 2) ? 1 : (ary_size)[3]
AryWidth  = (ary_size)[1]
AryHeight = (ary_size)[2]
TotHeight = AryHeight * numBlk

IF (numBlk GT 1) THEN $
   DataAry = REFORM(DataAry, [AryWidth, TotHeight], /OVERWRITE) 

;----------------------------------------------------------------------------
; If we don't want to offset the blocks, then we're done.
;----------------------------------------------------------------------------

IF (OffsetBlks EQ 0 OR numBlk EQ 1) THEN BEGIN
   ProdStruct = 0
   Status = 0
   RETURN
ENDIF

;----------------------------------------------------------------------------
; Get the cumulative block offsets that were loaded from L1B2 AN file at
; onset and are applicable to all MISR products for the same orbit. Offsets
; are stored as multiples of 275 m pixels, and 64 pixels is the basic block
; offset value. Cumulative values represent the number of pixels that blocks
; must be moved left or right as the block number increases. Negative values
; means move toward the left. 
;----------------------------------------------------------------------------

LoadedBlkBeg = CoordStruct.(Whichorbit).BlkBeg - 1
LoadedBlkEnd = CoordStruct.(Whichorbit).BlkEnd - 1
RequestBlkBeg = ProdStruct.Starts[2]
RequestBlkEnd = RequestBlkBeg + ProdStruct.Dims[2] - 1
begndx = RequestBlkBeg - LoadedBlkBeg
endndx = RequestBlkEnd - LoadedBlkBeg
blk_offsets = REFORM((*CoordStruct.(Whichorbit).BlkPixOffset)[begndx:endndx,1])

;----------------------------------------------------------------------------
; Compute the cumulative offsets for this data field's resolution. Handle the
; stereo wind products specially, because the pixel size is > 64 L1B2 pixels.
;----------------------------------------------------------------------------

blk_offsets = blk_offsets * AryWidth / 2048

ExpWidthLeft  = MIN(blk_offsets)
ExpWidthRight = MAX(blk_offsets)

;----------------------------------------------------------------------------
; Create a second array to concatenate with each block of the data array.
; When added, the resulting image width is large enough to accommodate the
; block offsets. Fill the array with the appropriate bad value indicators.
; Concatenate the arrays. The data array is on the right.
;----------------------------------------------------------------------------

IF (ExpWidthLeft LT 0) THEN BEGIN
   data_type = (ary_size)[N_ELEMENTS(ary_size)-2]
   ExpandAry = MAKE_ARRAY(ABS(ExpWidthLeft), TotHeight, TYPE=data_type, $
                          VALUE=BadValue)
   DataAry   = [ExpandAry, DataAry]
   ExpandAry = 0
ENDIF

IF (ExpWidthRight GT 0) THEN BEGIN
   data_type = (ary_size)[N_ELEMENTS(ary_size)-2]
   ExpandAry = MAKE_ARRAY(ExpWidthRight, TotHeight, TYPE=data_type, $
                          VALUE=BadValue)
   DataAry   = [DataAry, ExpandAry]
   ExpandAry = 0
ENDIF

;----------------------------------------------------------------------------
; Apply the cumulative block offsets to create a smooth swath image by moving
; data left within each block by the offset appropriate to the resolution.
;----------------------------------------------------------------------------

IF (numBlk GT 1) THEN BEGIN
   FOR iblk=0,numBlk-1 DO BEGIN
      ibeg = AryHeight * iblk
      iend = AryHeight * (iblk + 1) - 1
      blk_data = REFORM(DataAry[*,ibeg:iend])
      blk_data = SHIFT(blk_data, [blk_offsets[iblk],0])
      DataAry[*,ibeg:iend] = blk_data
   ENDFOR
ENDIF

blk_offsets = 0
blk_data = 0
ProdStruct = 0

;----------------------------------------------------------------------------
; The next block is for debugging only.
;----------------------------------------------------------------------------

IF (DO_DEBUG) THEN BEGIN
   IF (BadValue LE 0) THEN ndxs = WHERE(DataAry LE BadValue, numndxs)
   IF (BadValue GT 0) THEN ndxs = WHERE(DataAry GE BadValue, numndxs)
   IF (numndxs GT 0) THEN DataAry[ndxs] = 0
   PRINT, ' '
   PRINT, FieldName
   nelem = STRTRIM(STRING(N_ELEMENTS(SIZE(DataAry))),2) + 'I7'
   PRINT, FORMAT='(A,'+nelem+',A,'+nelem+')', 'SIZE= ', SIZE(DataAry), $
          ' Dims= ', edges
   PRINT, 'MIN= ', MIN(DataAry), '  MAX= ', MAX(DataAry), '  FillValue= ', $
          BadValue
   edges = 0

   Magnify = FLOOR(!KON.Misc.ScreenY / TotHeight)
   Mag = 1
   IF (Magnify GT  2) THEN Mag =  2
   IF (Magnify GT  4) THEN Mag =  4
   IF (Magnify GT  8) THEN Mag =  8
   IF (Magnify GT 16) THEN Mag = 16
   WINDOW, XSIZE=(AryWidth+ABS(ExpWidthLeft)+ExpWidthRight)*Mag, $
           YSIZE=TotHeight*Mag, TITLE=FieldName, /FREE
   TVSCL, REBIN(DataAry, (AryWidth+ABS(ExpWidthLeft)+ExpWidthRight)*Mag, $
                TotHeight*Mag, /SAMPLE), /ORDER
   rtrn = DIALOG_MESSAGE('Click when ready.', /INFO, /CENTER)
   pwin = !D.WINDOW
   SafeWDELETE, pwin, didit
ENDIF

edges = 0

Status = 0

END  ;  LoadHdfData

;***************************************************************************
PRO GetSwathMinMaxValues, DataType, MinVal, MaxVal
;***************************************************************************

COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON alb_data, Local_Albedo, Expans_Albedo, Restrict_Albedo

;----------------------------------------------------------------------------
; Compute the min and max values.
;----------------------------------------------------------------------------

MinVal =  9999.0
MaxVal = -9999.0

CASE DataType OF
   !KON.DataProd.TYPE_TERRAIN_HT   : product = Terrain_Hts
   !KON.DataProd.TYPE_LANDH2O_MASK : product = Land_Water_Mask
   !KON.DataProd.TYPE_BIOME_MAP    : product = Biome_grid
   !KON.DataProd.TYPE_SUN_ZENITH   : product = Sun_Zenith
   !KON.DataProd.TYPE_SUN_AZIMUTH  : product = Sun_Azimuth
   !KON.DataProd.TYPE_CAM_ZEN_DF   : product = REFORM(Cam_Zenith[0,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_CF   : product = REFORM(Cam_Zenith[1,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_BF   : product = REFORM(Cam_Zenith[2,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_AF   : product = REFORM(Cam_Zenith[3,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_AN   : product = REFORM(Cam_Zenith[4,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_AA   : product = REFORM(Cam_Zenith[5,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_BA   : product = REFORM(Cam_Zenith[6,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_CA   : product = REFORM(Cam_Zenith[7,*,*])
   !KON.DataProd.TYPE_CAM_ZEN_DA   : product = REFORM(Cam_Zenith[8,*,*])
   !KON.DataProd.TYPE_CAM_AZI_DF   : product = REFORM(Cam_Azimuth[0,*,*])
   !KON.DataProd.TYPE_CAM_AZI_CF   : product = REFORM(Cam_Azimuth[1,*,*])
   !KON.DataProd.TYPE_CAM_AZI_BF   : product = REFORM(Cam_Azimuth[2,*,*])
   !KON.DataProd.TYPE_CAM_AZI_AF   : product = REFORM(Cam_Azimuth[3,*,*])
   !KON.DataProd.TYPE_CAM_AZI_AN   : product = REFORM(Cam_Azimuth[4,*,*])
   !KON.DataProd.TYPE_CAM_AZI_AA   : product = REFORM(Cam_Azimuth[5,*,*])
   !KON.DataProd.TYPE_CAM_AZI_BA   : product = REFORM(Cam_Azimuth[6,*,*])
   !KON.DataProd.TYPE_CAM_AZI_CA   : product = REFORM(Cam_Azimuth[7,*,*])
   !KON.DataProd.TYPE_CAM_AZI_DA   : product = REFORM(Cam_Azimuth[8,*,*])
   !KON.DataProd.TYPE_CAM_SCT_DF   : product = REFORM(Cam_Scatter[0,*,*])
   !KON.DataProd.TYPE_CAM_SCT_CF   : product = REFORM(Cam_Scatter[1,*,*])
   !KON.DataProd.TYPE_CAM_SCT_BF   : product = REFORM(Cam_Scatter[2,*,*])
   !KON.DataProd.TYPE_CAM_SCT_AF   : product = REFORM(Cam_Scatter[3,*,*])
   !KON.DataProd.TYPE_CAM_SCT_AN   : product = REFORM(Cam_Scatter[4,*,*])
   !KON.DataProd.TYPE_CAM_SCT_AA   : product = REFORM(Cam_Scatter[5,*,*])
   !KON.DataProd.TYPE_CAM_SCT_BA   : product = REFORM(Cam_Scatter[6,*,*])
   !KON.DataProd.TYPE_CAM_SCT_CA   : product = REFORM(Cam_Scatter[7,*,*])
   !KON.DataProd.TYPE_CAM_SCT_DA   : product = REFORM(Cam_Scatter[8,*,*])
   !KON.DataProd.TYPE_CAM_GLT_DF   : product = REFORM(Cam_Glitter[0,*,*])
   !KON.DataProd.TYPE_CAM_GLT_CF   : product = REFORM(Cam_Glitter[1,*,*])
   !KON.DataProd.TYPE_CAM_GLT_BF   : product = REFORM(Cam_Glitter[2,*,*])
   !KON.DataProd.TYPE_CAM_GLT_AF   : product = REFORM(Cam_Glitter[3,*,*])
   !KON.DataProd.TYPE_CAM_GLT_AN   : product = REFORM(Cam_Glitter[4,*,*])
   !KON.DataProd.TYPE_CAM_GLT_AA   : product = REFORM(Cam_Glitter[5,*,*])
   !KON.DataProd.TYPE_CAM_GLT_BA   : product = REFORM(Cam_Glitter[6,*,*])
   !KON.DataProd.TYPE_CAM_GLT_CA   : product = REFORM(Cam_Glitter[7,*,*])
   !KON.DataProd.TYPE_CAM_GLT_DA   : product = REFORM(Cam_Glitter[8,*,*])
   !KON.DataProd.TYPE_AER_BE_TAU_B : product = REFORM(Tau_BE_Grid[0,*,*])
   !KON.DataProd.TYPE_AER_BE_TAU_G : product = REFORM(Tau_BE_Grid[1,*,*])
   !KON.DataProd.TYPE_AER_BE_TAU_R : product = REFORM(Tau_BE_Grid[2,*,*])
   !KON.DataProd.TYPE_AER_BE_TAU_N : product = REFORM(Tau_BE_Grid[3,*,*])
   !KON.DataProd.TYPE_AER_LR_TAU_B : product = REFORM(Tau_LR_Grid[0,*,*])
   !KON.DataProd.TYPE_AER_LR_TAU_G : product = REFORM(Tau_LR_Grid[1,*,*])
   !KON.DataProd.TYPE_AER_LR_TAU_R : product = REFORM(Tau_LR_Grid[2,*,*])
   !KON.DataProd.TYPE_AER_LR_TAU_N : product = REFORM(Tau_LR_Grid[3,*,*])
   !KON.DataProd.TYPE_AER_SSA_BLU  : product = REFORM(Ssa_Grid[0,*,*])
   !KON.DataProd.TYPE_AER_SSA_GRN  : product = REFORM(Ssa_Grid[1,*,*])
   !KON.DataProd.TYPE_AER_SSA_RED  : product = REFORM(Ssa_Grid[2,*,*])
   !KON.DataProd.TYPE_AER_SSA_NIR  : product = REFORM(Ssa_Grid[3,*,*])
   !KON.DataProd.TYPE_AER_BE_ANGXP : product = Angexp_BE_Grid
   !KON.DataProd.TYPE_AER_LR_ANGXP : product = Angexp_LR_Grid
   !KON.DataProd.TYPE_AER_FRC_SML  : product = REFORM(Taufrac_Grid[0,*,*])
   !KON.DataProd.TYPE_AER_FRC_MED  : product = REFORM(Taufrac_Grid[1,*,*])
   !KON.DataProd.TYPE_AER_FRC_LRG  : product = REFORM(Taufrac_Grid[2,*,*])
   !KON.DataProd.TYPE_AER_FRC_SPH  : product = REFORM(Taufrac_Grid[3,*,*])
   !KON.DataProd.TYPE_AER_FRC_NOS  : product = REFORM(Taufrac_Grid[4,*,*])
   !KON.DataProd.TYPE_LND_BHR_BLU  : product = REFORM(BHR_Grid[0,*,*])
   !KON.DataProd.TYPE_LND_BHR_GRN  : product = REFORM(BHR_Grid[1,*,*])
   !KON.DataProd.TYPE_LND_BHR_RED  : product = REFORM(BHR_Grid[2,*,*])
   !KON.DataProd.TYPE_LND_BHR_NIR  : product = REFORM(BHR_Grid[3,*,*])
   !KON.DataProd.TYPE_LND_DHR_BLU  : product = REFORM(DHR_Grid[0,*,*])
   !KON.DataProd.TYPE_LND_DHR_GRN  : product = REFORM(DHR_Grid[1,*,*])
   !KON.DataProd.TYPE_LND_DHR_RED  : product = REFORM(DHR_Grid[2,*,*])
   !KON.DataProd.TYPE_LND_DHR_NIR  : product = REFORM(DHR_Grid[3,*,*])
   !KON.DataProd.TYPE_LND_NDVI     : product = NDVI_Grid
   !KON.DataProd.TYPE_LND_RPV1     : product = REFORM(RPV_Grid[0,*,*])
   !KON.DataProd.TYPE_LND_RPV2     : product = REFORM(RPV_Grid[1,*,*])
   !KON.DataProd.TYPE_LND_RPV3     : product = REFORM(RPV_Grid[2,*,*])
   !KON.DataProd.TYPE_STER_ZEROHT  : product = STER_ZeroHts
   !KON.DataProd.TYPE_STER_CORRHT  : product = STER_CorrHts
   !KON.DataProd.TYPE_STER_SDCM    : product = STER_SDCM
   !KON.DataProd.TYPE_STER_WNDCRS  : product = STER_WndCrossHi > STER_WndCrossLo
   !KON.DataProd.TYPE_STER_WNDALG  : product = STER_WndAlongHi > STER_WndAlongLo
   !KON.DataProd.TYPE_CLDZ_ZEROHT  : product = CLDZ_ZeroHts
   !KON.DataProd.TYPE_CLDZ_WNDCRS  : product = CLDZ_WndCross
   !KON.DataProd.TYPE_CLDZ_CLDMSK  : product = CLDZ_CldMsk
   !KON.DataProd.TYPE_CLDC_CORRHT  : product = CLDC_CorrHts
   !KON.DataProd.TYPE_CLDC_WNDCRS  : product = CLDC_WndCross
   !KON.DataProd.TYPE_CLDC_SDCM    : product = CLDC_SDCM
   !KON.DataProd.TYPE_CLD_MOTNHT   : product = CLD_MotionHts
   !KON.DataProd.TYPE_CLD_WNDCRS   : product = CLD_WndCross
   !KON.DataProd.TYPE_CLD_WNDALG   : product = CLD_WndAlong
   !KON.DataProd.TYPE_CLD_CLDMSK   : product = CLD_CldMsk
   !KON.DataProd.TYPE_CLASS_SMOKE  : product = Smoke_Confid
   !KON.DataProd.TYPE_CLASS_DUST   : product = Dust_Confid
   !KON.DataProd.TYPE_CLASS_CLOUD  : product = Cloud_Confid
   !KON.DataProd.TYPE_CLASS_CLEAR  : product = Land_Confid
   !KON.DataProd.TYPE_ALB_LOC_BLU  : product = REFORM(Local_Albedo[0,*,*])
   !KON.DataProd.TYPE_ALB_LOC_GRN  : product = REFORM(Local_Albedo[1,*,*])
   !KON.DataProd.TYPE_ALB_LOC_RED  : product = REFORM(Local_Albedo[2,*,*])
   !KON.DataProd.TYPE_ALB_LOC_NIR  : product = REFORM(Local_Albedo[3,*,*])
   !KON.DataProd.TYPE_ALB_LOC_BRD  : product = REFORM(Local_Albedo[4,*,*])
   !KON.DataProd.TYPE_ALB_RES_BLU  : product = REFORM(Restrict_Albedo[0,*,*])
   !KON.DataProd.TYPE_ALB_RES_GRN  : product = REFORM(Restrict_Albedo[1,*,*])
   !KON.DataProd.TYPE_ALB_RES_RED  : product = REFORM(Restrict_Albedo[2,*,*])
   !KON.DataProd.TYPE_ALB_RES_NIR  : product = REFORM(Restrict_Albedo[3,*,*])
   !KON.DataProd.TYPE_ALB_RES_BRD  : product = REFORM(Restrict_Albedo[4,*,*])
   !KON.DataProd.TYPE_ALB_EXP_BLU  : product = REFORM(Expans_Albedo[0,*,*])
   !KON.DataProd.TYPE_ALB_EXP_GRN  : product = REFORM(Expans_Albedo[1,*,*])
   !KON.DataProd.TYPE_ALB_EXP_RED  : product = REFORM(Expans_Albedo[1,*,*])
   !KON.DataProd.TYPE_ALB_EXP_NIR  : product = REFORM(Expans_Albedo[1,*,*])
   !KON.DataProd.TYPE_ALB_EXP_BRD  : product = REFORM(Expans_Albedo[1,*,*])
   ELSE : product = []
ENDCASE

IF (product EQ []) THEN RETURN

ndxs = WHERE(product NE !VAR.DataProd[DataType].BadVal, numndxs)
IF (numndxs GT 0) THEN MinVal = MIN(product[ndxs], MAX=MaxVal)

product = 0

END  ;  GetSwathMinMaxValues

