;===========================================================================
;                                                                          =
;                               MINX Accessory                             =
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
; This code converts older MINX .txt files to the new format first used in
; the global 2008 project completed in 2014. It fixes fields and updates   
; the plume or region name field. It also renames all the files in a project
; so all orbit components of the names are fixed length and similarly with
; all block number components and all plume number components. The end result
; is a new project directory with all files copied, updated and renamed. It
; also verifies that the copy was successful, so the old project directory
; can be deleted with confidence.
;***************************************************************************

;---------------------------------------------------------------------------
; Convert a land cover type classification from a MODIS MCD12C1 file for
; biome type IGBP and convert it to a binary file.
;
; MCD12C1 file specifications are:
;   - one file for each year from 2001-2012 (as of 8/2014)
;   - files are each ~1.3 GBytes
;   - 10 grids in each file
;   - only one is used (SDS name = Majority_Land_Cover_Type_1)
;   - each grid is 8-bit unsigned integer
;   - grids are geographic mosaics with resolution = 0.05 degrees
;   - therefore each grid is 7200 x 3600 in size (25,920,000 bytes)
;   - pixels vary in physical size from ~5600 x 5600 m at the equator to
;     ~2800 x 5600 m at 60 degrees latitude
;   - 0,0 map origin is at lon = -180 and lat = +90 (lat is inverted)
;   - home website is:
;     https://lpdaac.usgs.gov/products/modis_products_table/mcd12c1
;
; V051 MODIS Land Cover Classification Types:
;   Class   IGBP (Type 1)
;   -----   -------------
;     0	    Water	 					
;     1     Evergreen Needleleaf forest
;     2     Evergreen Broadleaf forest
;     3     Deciduous Needleleaf forest
;     4     Deciduous Broadleaf forest
;     5     Mixed forest
;     6     Closed shrublands
;     7     Open shrublands
;     8     Woody savannas
;     9     Savannas
;    10     Grasslands
;    11     Permanent wetlands
;    12     Croplands
;    13     Urban and built-up
;    14     Cropland/Natural veg mosaic
;    15     Snow and ice
;    16     Barren or sparsely vegetated
;   255     Fill Value/Unclassified
;---------------------------------------------------------------------------

;***************************************************************************
PRO Read_MODIS_LandType_Grid, BiomeDfltDir, BiomeDfltFile, BiomeGridfile, $
                              BiomeGridSpacing, BiomeGrid, Retval
;***************************************************************************
; Laod the MODIS land type grid extracted from the MCD12C1 product for the
; appropriate year. Use the grid containing the IGBP classification. The
; extraction and binary grid file creation must have been accomplished
; earlier using the Extract_MODIS_LandType_Grid procedure.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1
num_lon = 7200L
num_lat = 3600L

BiomeGrid = BYTARR(num_lon, num_lat)

;---------------------------------------------------------------------------
; Get the name of the MODIS MCD12C1 file for the desired year.
;---------------------------------------------------------------------------

;DLNDLN restore
BiomeGridfile = BiomeDfltDir + BiomeDfltFile
;BiomeGridfile = DIALOG_PICKFILE(TITLE='Select input MODIS biome grid file', $
;                       FILTER='*.dat', PATH=BiomeDfltDir, /MUST_EXIST, $
;                       GET_PATH=biome_gridpath_name, FILE=BiomeDfltFile)
  
IF (BiomeGridfile EQ '') THEN RETURN
BiomeGridSpacing = 0.05 ; in degrees

;---------------------------------------------------------------------------
; Read the binary file.
;---------------------------------------------------------------------------

OPENR, UnitF, BiomeGridfile, /GET_LUN
READU, UnitF, BiomeGrid, TRANSFER_COUNT=num_elem
FREE_LUN, UnitF

siz_xy = SIZE(BiomeGrid)

;DLNDLN restore
;mssg = ['Number of grid elements read = ' + STRTRIM(STRING(num_elem),2), $
;        'Array dimensions = ' + STRTRIM(STRING(siz_xy[1]),2) + ' x ' + $
;        STRTRIM(STRING(siz_xy[2]),2)]
;rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)

Retval = 0
   
END  ;  Read_MODIS_LandType_Grid

;***************************************************************************
PRO Get_BiomeType_At_LatLong, BiomeGrid, BiomeGridSpacing, Lon, Lat, $
                              BiomeTypeCode, BiomeName
;***************************************************************************
; Return the type code and name of the biome at the specified lon/lat
; coordinate pair.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

InvalidValue = 17
NumShells = 5   ; The number of squares or shells of increasing size around
                ; retrieved bad class location to search for a replacement.
                ; Each shell's pixels are ~5 km wide!

BiomeTypeNames = $
   ['Water', 'Evergreen Needleleaf forest', 'Evergreen Broadleaf forest', $
    'Deciduous Needleleaf forest', 'Deciduous Broadleaf forest', $
    'Mixed forest', 'Closed shrublands', 'Open shrublands', $
    'Woody savannas', 'Savannas', 'Grasslands', 'Permanent wetlands', $
    'Croplands', 'Urban and built-up', 'Cropland/Natural veg mosaic', $
    'Snow and ice', 'Barren or sparsely vegetated', 'Unclassified']
   
;---------------------------------------------------------------------------
; Convert lat/lon to array indices. map origin is at lon = -180 and lat = +90
;---------------------------------------------------------------------------

num_lon = (SIZE(BiomeGrid))[1]
num_lat = (SIZE(BiomeGrid))[2]

lon_coeff = (360.0 - BiomeGridSpacing) / (360.0 * BiomeGridSpacing)
lat_coeff = (180.0 - BiomeGridSpacing) / (180.0 * BiomeGridSpacing) * (-1.0)

lon_ndx = FLOOR(Lon * lon_coeff + (num_lon - 1.0) / 2.0)
lat_ndx = FLOOR(Lat * lat_coeff + (num_lat - 1.0) / 2.0)

;---------------------------------------------------------------------------
; Get the biome type code at the specified index pair.
;---------------------------------------------------------------------------

BiomeTypeCode = BiomeGrid[lon_ndx, lat_ndx]

;---------------------------------------------------------------------------
; Refine the biome type code by changing every occurrence of the rejected
; classes - water (0) and snow (15) - to the class of the nearest region
; that is not one of these.
;---------------------------------------------------------------------------

done_flag = 0
best_distance = 99999.0
best_type_code = BiomeTypeCode

IF (BiomeTypeCode EQ 0 OR BiomeTypeCode EQ 15) THEN BEGIN ; water or snow

   best_type_code = 255  ; if no replacement is found, use Unclassified
   
   ;------------------------------------------------------------------------
   ; Loop over square groups of biome pixels (shells) surrounding the point
   ; in order of increasing distance NumShells times. Stop as soon as a
   ; shell has been completed and a biome pixel is found in the shell that
   ; is not one of the rejected biomes. Use the biome class that is closest
   ; to the center point to replace the center point's rejected biome class.
   ;------------------------------------------------------------------------
   
   n_pix = 8 * NumShells + 4
   xlon = INTARR(n_pix)
   ylat = INTARR(n_pix)
   
   FOR ishell=1,NumShells DO BEGIN
   
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
      
         new_type_code = BiomeGrid[xlon[ipnt], ylat[ipnt]]
         
         IF (new_type_code NE 0 AND new_type_code NE 15) THEN BEGIN ; water or snow
            
            ;---------------------------------------------------------------
            ; If this biome pixel not one of the rejected classes, find its
            ; distance from the center point and process further. We need to
            ; convert lat/lon indices at biome pixel centers to meters first.
            ;---------------------------------------------------------------
            
            new_dist = MAP_2POINTS((xlon[ipnt] * BiomeGridSpacing - 180.0D), $
                                   (ylat[ipnt] * BiomeGridSpacing -  90.0D), $
                                      (lon_ndx * BiomeGridSpacing - 180.0D), $
                                      (lat_ndx * BiomeGridSpacing -  90.0D), $
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
ENDIF

xlon = 0
ylat = 0

;---------------------------------------------------------------------------
; Get the biome type code at specified index pair, then the biome name.
;---------------------------------------------------------------------------

BiomeTypeCode = best_type_code

IF (BiomeTypeCode GE InvalidValue) THEN BEGIN
   BiomeName = BiomeTypeNames[InvalidValue]
ENDIF ELSE BEGIN
   BiomeName = BiomeTypeNames[BiomeTypeCode]
ENDELSE

END  ;  Get_BiomeType_At_LatLong

;***************************************************************************
PRO ComputePlumeQuality1, NumHtPnts, FracHtArea, RMS_dev, MaxHt, QualFlag
;***************************************************************************
; Compute a quality factor for this region. The parameters were developed
; entirely by "tuning" - repeatedly running many cases with different
; combinations of the parameters.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Define thresholds (G = Good, F = Fair, P = Poor).
;---------------------------------------------------------------------------

NUM_PNTS_MIN =  5.00 ; fewer points than this immediately returns Poor
NUM_PNTS_MAX = 40.00 ; more points than this cannot improve the rating

FRAC_AREA_MIN = 0.07 ; area-filled fraction less than this returns Poor
FRAC_AREA_MAX = 0.40 ; a larger fraction than this cannot improve rating

FRAC_RMS_MIN = 0.10  ; smaller RMS variation than this returns Poor
FRAC_RMS_MAX = 2.00  ; a larger value than this cannot improve rating

QA_GOOD_GF = 0.85    ; min value of composite score to qualify as GOOD.
QA_POOR_FP = 0.10    ; max value of composite score to qualify as POOR.

;---------------------------------------------------------------------------
; If any minimum criterion is not met, return 'POOR'. Any max height less
; than 150 m above terrain is bad!
;---------------------------------------------------------------------------

QualFlag = 'Poor'

IF (MaxHt LT 0.150 OR NumHtPnts LT NUM_PNTS_MIN OR $
    FracHtArea LT FRAC_AREA_MIN OR RMS_dev LE 0.0 OR $
    (RMS_dev GT 1.0 / FRAC_RMS_MIN)) THEN RETURN

;---------------------------------------------------------------------------
; Calculate the quality flag and return it.
;---------------------------------------------------------------------------

num_ht_pts = (ALOG10(NumHtPnts - NUM_PNTS_MIN + 2) / ALOG10(NUM_PNTS_MAX)) < 1.0
frac_filled = ALOG10(FracHtArea * 10) > 0.1
rms_dev_hts = (FRAC_RMS_MIN / RMS_dev) < FRAC_RMS_MAX

composite_score = num_ht_pts * frac_filled * rms_dev_hts

QualFlag = (composite_score GE QA_GOOD_GF) ? 'Good' : $
           (composite_score LE QA_POOR_FP) ? 'Poor' : 'Fair'
      
END  ;  ComputePlumeQuality1

;***************************************************************************
PRO Get_RGN_Polygons1, NumGeoRgn, poly_ptr, rgn_names
;***************************************************************************
; Create and fill an array of data structures, each containing a description
; of a geographic region into which plume data will be partitioned.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE
             
;---------------------------------------------------------------------------
; Initialize the names of the geographic regions and the lat/lon coords of
; the map polygons defining them. Seven regions have been defined: Africa,
; Australia, SouthwestEurasia, NorthAmerica, BorealEurasia, SouthAmerica
; and SouthAsia.
;---------------------------------------------------------------------------

rgn_names = ['Africa', 'Australia', 'SouthwestEurasia', 'NorthAmerica', $
             'BorealEurasia', 'SouthAmerica', 'SouthAsia']

NumGeoRgn = N_ELEMENTS(rgn_names)

poly_ptr = PTRARR(NumGeoRgn)

poly_ptr[0] = PTR_NEW( $  ;  Africa (clockwise)
   [[ 36.0, -20.00],[ 36.0,  -5.80],[ 36.0,  -2.10],[ 38.5,   5.00], $
    [ 38.4,   9.00],[ 34.3,  17.20],[ 34.1,  27.50],[ 32.2,  31.60], $
    [ 26.5,  34.40],[ 22.5,  37.70],[ 17.5,  40.00],[ 11.7,  43.90], $
    [ 15.0,  60.00],[-13.0,  75.00],[-40.0,  60.00],[-85.0,  60.00], $
    [-85.0,  25.00],[-85.0, -10.00],[-40.0, -15.00],[-10.0, -18.00], $
    [ 30.0, -40.00],[ 36.0, -20.00]])
poly_ptr[1] = PTR_NEW( $  ;  Australia (clockwise)
   [[-13.0,  75.00],[-13.0, 120.00],[-10.0, 130.00],[-10.2, 142.50], $
    [-13.0, 150.00],[-13.0, 179.99],[-18.0, 179.99],[-23.0, 179.99], $
    [-29.0, 179.99],[-35.0, 179.99],[-41.0, 179.99],[-49.0, 179.99], $
    [-57.0, 179.99],[-65.0, 179.99],[-73.0, 179.99],[-81.0, 179.99], $
    [-85.0, 179.99],[-85.0,  60.00],[-40.0,  60.00],[-13.0,  75.00]])
poly_ptr[2] = PTR_NEW( $  ;  Southwest Eurasia (counterclockwise)
   [[ 41.2,  29.50],[ 44.6,  38.00],[ 42.5,  46.20],[ 47.5,  42.50], $
    [ 51.0,  46.50],[ 51.0,  58.00],[ 52.5,  60.00],[ 53.0,  64.00], $
    [ 51.5,  70.50],[ 54.0,  71.00],[ 53.0,  77.00],[ 50.0,  82.00], $
    [ 50.5,  85.00],[ 53.3,  87.30],[ 55.5,  85.70],[ 57.5,  70.00], $
    [ 56.0,  65.00],[ 57.0,  61.50],[ 52.0,  57.00],[ 53.0,  56.50], $
    [ 56.0,  56.50],[ 57.5,  47.50],[ 54.5,  42.00],[ 55.5,  38.00], $
    [ 53.5,  34.50],[ 56.5,  28.00],[ 57.0,  20.00],[ 55.3,  15.50], $
    [ 57.1,  11.20],[ 58.4,  10.50],[ 56.0,   4.00],[ 62.0,   0.00], $
    [ 58.0, -20.00],[ 50.0, -40.00],[ 40.0, -40.00],[ 30.0, -40.00], $
    [ 36.0, -20.00],[ 36.0,  -5.80],[ 36.0,  -2.10],[ 38.5,   5.00], $
    [ 38.4,   9.00],[ 34.3,  17.20],[ 34.1,  27.50],[ 39.8,  24.50], $
    [ 40.6,  28.00],[ 41.2,  29.50]])
poly_ptr[3] = PTR_NEW( $  ;  North America (clockwise)
   [[ 36.0,-179.99],[ 44.0,-179.99],[ 52.0,-179.99],[ 60.0,-179.99], $
    [ 65.0,-179.99],[ 70.0,-179.99],[ 77.0,-179.99],[ 85.0,-179.99], $
    [ 85.0,-130.00],[ 85.0,-100.00],[ 85.0, -80.00],[ 85.0, -60.00], $
    [ 85.0, -40.00],[ 77.0, -40.00],[ 71.0, -40.00],[ 64.0, -40.00], $
    [ 56.0, -40.00],[ 50.0, -40.00],[ 40.0, -40.00],[ 30.0, -40.00], $
    [ 19.0, -60.00],[ 10.5, -81.20],[  8.4, -79.40],[  6.0, -80.00], $
    [  6.0,-130.00],[  6.0,-179.99],[ 14.0,-179.99],[ 23.0,-179.99], $
    [ 29.5,-179.99],[ 36.0,-179.99]])
poly_ptr[4] = PTR_NEW( $  ;  Boreal Eurasia (clockwise)
   [[ 47.0, 179.99],[ 47.0, 160.00],[ 50.0, 150.00],[ 57.0, 145.00], $
    [ 55.0, 140.00],[ 51.0, 132.00],[ 54.0, 130.00],[ 53.5, 119.00], $
    [ 52.5, 118.00],[ 51.0, 111.00],[ 50.5, 104.50],[ 53.0,  99.50], $
    [ 50.0,  98.00],[ 52.0,  93.00],[ 50.5,  85.00],[ 53.3,  87.30], $
    [ 55.5,  85.70],[ 57.5,  70.00],[ 56.0,  65.00],[ 57.0,  61.50], $
    [ 52.0,  57.00],[ 53.0,  56.50],[ 56.0,  56.50],[ 57.5,  47.50], $
    [ 54.5,  42.00],[ 55.5,  38.00],[ 53.5,  34.50],[ 56.5,  28.00], $
    [ 57.0,  20.00],[ 55.3,  15.50],[ 57.1,  11.20],[ 58.4,  10.50], $
    [ 56.0,   4.00],[ 62.0,   0.00],[ 58.0, -20.00],[ 50.0, -40.00], $
    [ 56.0, -40.00],[ 64.0, -40.00],[ 71.0, -40.00],[ 69.0, -40.00], $
    [ 77.0, -40.00],[ 85.0, -40.00],[ 85.0, -20.00],[ 85.0,   5.00], $
    [ 85.0,  30.00],[ 85.0,  55.00],[ 85.0,  80.00],[ 85.0, 105.00], $
    [ 85.0, 130.00],[ 85.0, 155.00],[ 85.0, 179.99],[ 78.0, 179.99], $
    [ 71.0, 179.99],[ 63.0, 179.99],[ 55.0, 179.99],[ 47.0, 179.99]])
poly_ptr[5] = PTR_NEW( $  ;  South America (clockwise)
   [[  6.0,-179.99],[  6.0, -80.00],[  8.4, -79.40],[ 10.5, -81.20], $
    [ 19.0, -60.00],[ 30.0, -40.00],[-10.0, -18.00],[-40.0, -15.00], $
    [-85.0, -10.00],[-85.0, -35.00],[-85.0, -60.00],[-85.0, -85.00], $
    [-85.0,-110.00],[-85.0,-135.00],[-85.0,-160.00],[-85.0,-179.99], $
    [-76.0,-179.99],[-68.0,-179.99],[-60.0,-179.99],[-52.0,-179.99], $
    [-44.0,-179.99],[-36.0,-179.99],[-29.0,-179.99],[-22.0,-179.99], $
    [-15.0,-179.99],[ -8.0,-179.99],[ -1.0,-179.99],[  6.0,-179.99]])
poly_ptr[6] = PTR_NEW( $  ;  South Asia (counterclockwise)
   [[-13.0, 150.00],[-13.0, 179.99],[ -4.0, 179.99],[  5.0, 179.99], $
    [ 14.0, 179.99],[ 22.0, 179.99],[ 30.0, 179.99],[ 38.0, 179.99], $
    [ 47.0, 179.99],[ 47.0, 160.00],[ 50.0, 150.00],[ 57.0, 145.00], $
    [ 55.0, 140.00],[ 51.0, 132.00],[ 54.0, 130.00],[ 53.5, 119.00], $
    [ 52.5, 118.00],[ 51.0, 111.00],[ 50.5, 104.50],[ 53.0,  99.50], $
    [ 50.0,  98.00],[ 52.0,  93.00],[ 50.5,  85.00],[ 50.0,  82.00], $
    [ 53.0,  77.00],[ 54.0,  71.00],[ 51.5,  70.50],[ 53.0,  64.00], $
    [ 52.5,  60.00],[ 51.0,  58.00],[ 51.0,  46.50],[ 47.5,  42.50], $
    [ 42.5,  46.20],[ 44.6,  38.00],[ 40.6,  28.00],[ 39.8,  24.50], $
    [ 34.1,  27.50],[ 32.2,  31.60],[ 26.5,  34.40],[ 22.5,  37.70], $
    [ 17.5,  40.00],[ 11.7,  43.90],[ 15.0,  60.00],[-13.0,  75.00], $
    [-13.0, 120.00],[-10.0, 130.00],[-10.2, 142.50],[-13.0, 150.00]])

END  ;  Get_RGN_Polygons1

;***************************************************************************
PRO UpdatePlumeName, OldPlumeName, NewPlumeName
;***************************************************************************
; Change the plume name from the old format to the new format by prefixing
; plume name components with 0s so it satisfies the requirement that all
; names to be fixed length.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

toks = STRSPLIT(OldPlumeName, '-', /EXTRACT, COUNT=numtoks)
orbit_numb = STRMID(toks[0], 1)
IF (STRLEN(orbit_numb) LT 6) THEN orbit_numb = '0' + orbit_numb
IF (STRLEN(orbit_numb) LT 6) THEN orbit_numb = '0' + orbit_numb
block_numb = STRMID(toks[1], 1)
IF (STRLEN(block_numb) LT 3) THEN block_numb = '0' + block_numb
IF (STRLEN(block_numb) LT 3) THEN block_numb = '0' + block_numb
plume_code = STRMID(toks[2], 0, 4)
plume_numb = STRMID(toks[2], 4)
IF (STRLEN(plume_numb) LT 2) THEN plume_numb = '0' + plume_numb
NewPlumeName = 'O' + orbit_numb + '-B' + block_numb + '-' + plume_code + $
               plume_numb
toks = 0

END  ;  UpdatePlumeName

;***************************************************************************
PRO RenamePlumeFilesForOrbit, NewDirectory, OldFilename, OldPlumeName, $
                              NumFilesRenamed, NumAnalyzeErrors, Retval
;***************************************************************************
; Find every plume file in the appropriate directory whose plume name
; component matches that in OldPlumeFile. Rename these files if needed to
; conform to the new constant length plume names. Return the number of files
; renamed.
;---------------------------------------------------------------------------
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumFilesRenamed = 0
Retval = -1

;---------------------------------------------------------------------------
; Create the plume's new name.
;---------------------------------------------------------------------------

UpdatePlumeName, OldPlumeName, new_plume_name

;---------------------------------------------------------------------------
; Get a list of the files containing the plume's name component.
;---------------------------------------------------------------------------

CD, NewDirectory
old_file_list1 = FILE_SEARCH('*' + OldPlumeName + '*')
num_file = N_ELEMENTS(old_file_list1)
old_file_list = STRARR(num_file)
num_new = 0
FOR ifile=0,num_file-1 DO BEGIN
   IF (STRMATCH(old_file_list1[ifile], OldPlumeName + '_*') OR $
       STRMATCH(old_file_list1[ifile], '*' + OldPlumeName + '.*')) THEN BEGIN
      old_file_list[num_new] = old_file_list1[ifile]
      num_new += 1
   ENDIF
ENDFOR
toks = 0
old_file_list1 = 0

;---------------------------------------------------------------------------
; Rename each file. Take care that the last numeric character in a file name
; is treated properly. E.g., make sure 1 and 11 are treated separately.
;---------------------------------------------------------------------------

FOR ifile=0,num_new-1 DO BEGIN
   old_file_name = NewDirectory
   new_file_name = NewDirectory
   
   toks = STRSPLIT(old_file_list[ifile], '_.', /EXTRACT, COUNT=numtoks)
   IF (toks[0] EQ OldPlumeName) THEN BEGIN
      IF (numtoks EQ 3) THEN BEGIN
         old_file_name += OldPlumeName + '_' + toks[1] + '.' + toks[2]
         new_file_name += new_plume_name + '_' + toks[1] + '.' + toks[2]
      ENDIF
      IF (numtoks EQ 4) THEN BEGIN
         old_file_name += OldPlumeName + '_' + toks[1] + '_' + $
                          toks[2] + '.' + toks[3]
         new_file_name += new_plume_name + '_' + toks[1] + '_' + $
                          toks[2] + '.' + toks[3]
      ENDIF
   ENDIF ELSE BEGIN
      old_file_name += toks[0] + '_' + OldPlumeName + '.' + toks[2]
      new_file_name += toks[0] + '_' + new_plume_name + '.' + toks[2]
   ENDELSE
   
   ;---------------------------------------------------------------------------
   ; Rename the file in place.
   ;---------------------------------------------------------------------------

   IF (OldPlumeName NE new_plume_name) THEN BEGIN
      FILE_MOVE, old_file_name, new_file_name
      NumFilesRenamed += 1
   ENDIF
ENDFOR

Retval = 0
toks = 0

old_file_list = 0

END  ;  RenamePlumeFilesForOrbit

;***************************************************************************
PRO CopyFilesForPlumeAsIs, ProjDir, NewProjDir, PlumeName
;***************************************************************************
; Copy all the files that match the plume name component of a plume .txt 
; file from one project directory to another. The new directory leaves the
; original in place to ensure the ability to recover in case of an error.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

CD, ProjDir
old_file_list1 = FILE_SEARCH('*' + PlumeName + '*')
num_file = N_ELEMENTS(old_file_list1)
old_file_list = STRARR(num_file)
num_new = 0

;---------------------------------------------------------------------------
; Get a list of the files containing the plume's name component.
;---------------------------------------------------------------------------

FOR ifile=0,num_file-1 DO BEGIN
   IF (STRMATCH(old_file_list1[ifile], PlumeName + '_*') OR $
      STRMATCH(old_file_list1[ifile], '*' + PlumeName + '.*')) THEN BEGIN
      old_file_list[num_new] = old_file_list1[ifile]
      num_new += 1
   ENDIF
ENDFOR
old_file_list1 = 0

;---------------------------------------------------------------------------
; Copy the files.
;---------------------------------------------------------------------------

FOR ifile=0,num_new-1 DO BEGIN
   old_plume_file = ProjDir + old_file_list[ifile]
   new_plume_file = NewProjDir + old_file_list[ifile]
   IF (~ FILE_TEST(NewProjDir, /DIRECTORY)) THEN $
      FILE_MKDIR, NewProjDir
   FILE_COPY, old_plume_file, new_plume_file
ENDFOR
old_file_list = 0

END  ;  CopyFilesForPlumeAsIs

;***************************************************************************
PRO SmoothRetrievedValues1, Lon, Lat, Terr_Values, ZWind_Values, $
                            Z_Values_In, Bad_Z_Value, Z_Values_Out, $
                            NdxsGood, RMS_Dev, Status
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

ZW_HTS_THRESH = 75.0
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
RMS_Dev = -9999.0

;---------------------------------------------------------------------------
; If this is wind-corrected data, check that the zero-wind heights are not
; near the terrain. When the wind direction is almost exactly along-track,
; heights near the terrain can be projected up several thousand meters!!
; This can also happen sporadically elsewhere.
;---------------------------------------------------------------------------

input_z_vals = FLOAT(Z_Values_In)
zw_ndxs = WHERE(ABS(ZWind_Values - Terr_Values) LT ZW_HTS_THRESH, zw_num)
IF (zw_num GT 0) THEN input_z_vals[zw_ndxs] = Bad_Z_Value
zw_ndxs = 0

;---------------------------------------------------------------------------
; Don't proceed unless there are enough good points.
;---------------------------------------------------------------------------

ndxgood = WHERE(input_z_vals GT Bad_Z_Value, numgood)

IF (numgood LT 5) THEN BEGIN
   Z_Values_Out = input_z_vals
   NdxsGood = ndxgood
   IF (numgood GT 0) THEN Status = numgood
   input_z_vals = 0
   ndxgood = 0
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Here's an interesting but simple heuristic for outlier removal:
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
   input_z_vals = 0
   ndxgood = 0
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Subset the lat/lon arrays with the good indexes. Then convert them to
; distances from their means in degrees and finally convert to distances from
; their means in km. Also compute the appropriate max value for the random
; perturbations (to avoid tesselation failure).
;---------------------------------------------------------------------------

coords_lon = DOUBLE(Lon[ndxgood])
coords_lat = DOUBLE(Lat[ndxgood])

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
; Subset the Z array using only the good indexes.
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

END  ;  SmoothRetrievedValues1

;***************************************************************************
PRO GetBestHeights1, xCoords, yCoords, RawHeights, SmoothedHts, BestHtMean, $
                     BestHtMed, BestHtMax, Status
;***************************************************************************
; Compute a best median and best top height (injection height?) for display
; on the profle, and return them for writing to the raw data file. For these
; calculations use filtered heights, and reject a few of the highest points
; as outliers (more than 3 std devs from the median). If there aren't enough
; points to compute a top, just use the median value of the filtered data.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Constants.
;---------------------------------------------------------------------------

MIN_INPTS_THRESH = 3
FUNC_COEF = [0.286135, 2.25916, 1.24014]
BADVALUE_REAL = -9999.0

;---------------------------------------------------------------------------
; Initialize.
;---------------------------------------------------------------------------

Status = -1
BestHtMean = BADVALUE_REAL
BestHtMed  = BADVALUE_REAL
BestHtMax  = BADVALUE_REAL

ndxs1 = WHERE(RawHeights  GT BADVALUE_REAL AND $
              SmoothedHts GT BADVALUE_REAL, numndxs1)
IF (numndxs1 LT MIN_INPTS_THRESH) THEN RETURN

raw_hts    = FLOAT(RawHeights[ndxs1])
smooth_hts = FLOAT(SmoothedHts[ndxs1])
BestHtMean = MEAN(smooth_hts)
BestHtMed  = MEDIAN(smooth_hts, /EVEN)
BestHtMax  = MAX(smooth_hts)

;---------------------------------------------------------------------------
; Compute the standard deviation threshold appropriate for the number of
; successfully retrieved points. Use the function below with these results
; e.g. at:  num pts input [ 1000,   200,    40,     8]
;           stddev thresh [ 2.20,  2.00,  1.80,  1.60]
;           equivalent to [0.986, 0.977, 0.964, 0.945] in cumulative prob.
;           apprx pts del [ 13.9,   4.6,   1.4,   0.4]
; This is applied only to points whose input smoothed values are larger than
; the surface fit values!
;---------------------------------------------------------------------------

stddev_thresh = FUNC_COEF[0] * ALOG10(FUNC_COEF[1] * numndxs1) + FUNC_COEF[2]

;---------------------------------------------------------------------------
; Compute best height value by removing points that are more than N standard
; deviations from the surface defined by the difference between the raw data
; and the smoothed data. The max of the remaining points is the best height.
;---------------------------------------------------------------------------

IF (numndxs1 GE MIN_INPTS_THRESH) THEN BEGIN
   surf_diff = raw_hts - smooth_hts
   
   std_dev = STDDEV(surf_diff)
   ndxs2 = WHERE(surf_diff GT (stddev_thresh * std_dev), numndxs2, $
                 COMPLEMENT=ndxs3, NCOMPLEMENT=numndxs3)
      
   IF (numndxs3 GE MIN_INPTS_THRESH) THEN BestHtMax = MAX(smooth_hts[ndxs3])
ENDIF

Status = 0

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

raw_hts = 0
smooth_hts = 0
surf_diff = 0
ndxs1 = 0
ndxs2 = 0
ndxs3 = 0
   
END  ;  GetBestHeights1

;***************************************************************************
PRO InsertEntryInArray, Array, Ndx, NewEntry
;***************************************************************************
; Insert a new entry into an existing array after the line referenced by Ndx.
; Shift all entries after Ndx up 1 entry and insert the new one. The array
; has just enough extra entries to permit letting them drop off the end.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

num_item = N_ELEMENTS(Array)
Array[Ndx+2:num_item-1] = Array[Ndx+1:num_item-2]
Array[Ndx+1] = NewEntry

END  ;  InsertEntryInArray

;***************************************************************************
PRO DeleteEntryInArray, Array, Ndx
;***************************************************************************
; Delete an existing entry in an array at the line referenced by Ndx. Shift
; all the entries after Ndx down 1 entry.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

num_item = N_ELEMENTS(Array)
Array[Ndx:num_item-2] = Array[Ndx+1:num_item-1]
   
END  ;  InsertEntryInArray

;***************************************************************************
PRO FindUnmatchedRandB, UnitG, ProjectDir, FileTypeString, NumFound, $
                        NumBadMatch, BadMatchList
;***************************************************************************
; Return a list of plumes where either red or blue band retrieval is missing.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Generate a list of raw text filenames for the project.
;---------------------------------------------------------------------------

CD, ProjectDir
file_list = FILE_SEARCH(FileTypeString)
NumFound = N_ELEMENTS(file_list)

BadMatchList = ['']
NumBadMatch = 0

;---------------------------------------------------------------------------
; Create a sorted list of plumes. This requires first removing the R or B
; code in the filenames so the red and blue will be adjacent.
;---------------------------------------------------------------------------

new_list = STRMID(file_list, 0, 30) + STRMID(file_list, 31, 7)
new_list = new_list[SORT(new_list)]
IF (NumFound MOD 2 GT 0) THEN BEGIN
   new_list = [new_list, '', '']
   NumFound += 1
ENDIF

;---------------------------------------------------------------------------
; Extract the R or B codes and make sure there are equal numbers.
;---------------------------------------------------------------------------

rb_list = STRMID(file_list, 30, 1)
ndx_R = WHERE(rb_list EQ 'R', num_R)
ndx_B = WHERE(rb_list EQ 'B', num_B)
IF (num_R NE num_B) THEN BEGIN
   BadMatchList = [BadMatchList, 'Red Blue mismatch']
   NumBadMatch += 1
ENDIF

;---------------------------------------------------------------------------
; Examine the "pairs" and make sure each plume has 2 copies (Red and Blue).
;---------------------------------------------------------------------------

FOR ipair=0,NumFound-2,2 DO BEGIN
   IF (new_list[ipair+1] NE new_list[ipair]) THEN BEGIN
      BadMatchList = [BadMatchList, new_list[ipair+1]]
      NumBadMatch += 1
      ipair -= 1
   ENDIF
ENDFOR

IF (NumBadMatch GT 0) THEN BadMatchList = BadMatchList[1:NumBadMatch]

file_list = 0
new_list = 0 
rb_list = 0
ndx_R = 0
ndx_B = 0

END  ;  FindUnmatchedRandB

;***************************************************************************
PRO TestFilenameLength, UnitG, ProjectDir, FileTypeString, ExpectedNameLen, $
                        ExpectedNumber, NumberFound, NumberBadLength, $
                        NumVerifyErrors
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumberBadLength = 0
NumberFound = 0

;---------------------------------------------------------------------------
; Generate a list of filenames for the specified file type and count them.
;---------------------------------------------------------------------------

CD, ProjectDir
file_list = FILE_SEARCH(FileTypeString)
NumberFound = N_ELEMENTS(file_list)
IF (NumberFound EQ 1 AND file_list[0] EQ '') THEN NumberFound = 0

IF (ExpectedNumber NE -9999 AND NumberFound NE ExpectedNumber) THEN BEGIN
   PRINTF, UnitG, 'Num expected / Num found for: ' + FileTypeString + ' = ' + $
           STRTRIM(STRING(ExpectedNumber),2) + ' / ' + $
           STRTRIM(STRING(NumberFound),2)
   NumVerifyErrors += 1

   IF (ExpectedNumber LE 1) THEN BEGIN
      file_list = 0
      RETURN
   ENDIF
ENDIF

;---------------------------------------------------------------------------
; Loop over all filenames and test the length of each against the expected
; length.
;---------------------------------------------------------------------------

FOR ifile=0,NumberFound-1 DO BEGIN
   name_len = STRLEN(file_list[ifile])
   IF (name_len NE ExpectedNameLen) THEN BEGIN
      PRINTF, UnitG, 'Bad name length for file: ' + file_list[ifile]
      NumberBadLength += 1
      NumVerifyErrors += 1
   ENDIF
ENDFOR

file_list = 0
   
END  ;  TestFilenameLength

;***************************************************************************
PRO ComputeTableValues, WC_OR_ZW, FileArray, RowData, BegLine, EndLine, $
                        RMS_dev, NumGoodHts, MeanHt, MedHt, MaxHt, Std_Dev
;***************************************************************************
; Compute a few parameters that need the zero-wind or wind-corrected value
; read from the header.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Std_Dev = -9999.0
RMS_dev = -9999.0
MeanHt = -9999
MedHt  = -9999
MaxHt  = -9999

;---------------------------------------------------------------------------
; For "clouds" (no wind-correction), there are 0s in the wind-corr height
; and wind fields. These must be changed to -9999 and -99.9 respectively.
;---------------------------------------------------------------------------

IF (WC_OR_ZW EQ 'ZW') THEN BEGIN
   badint = '-9999'
   badflt = '-99.9'
   FOR itbl=BegLine,EndLine-1 DO BEGIN
      line_text = FileArray[itbl]
      windcorr_ht = FLOAT(STRMID(line_text, 71, 5))
      IF (windcorr_ht EQ 0) THEN  STRPUT, line_text, badint, 70
      wind_cross = FLOAT(STRMID(line_text, 85, 5))
      IF (wind_cross EQ 0.0) THEN STRPUT, line_text, badflt, 84
      wind_along = FLOAT(STRMID(line_text, 91, 5))
      IF (wind_along EQ 0.0) THEN STRPUT, line_text, badflt, 90
      wind_total = FLOAT(STRMID(line_text, 97, 5))
      IF (wind_total EQ 0.0) THEN STRPUT, line_text, badflt, 96
      FileArray[itbl] = line_text
   ENDFOR
ENDIF

;---------------------------------------------------------------------------
; Use the Delaunay algorithm to smooth the retrieved heights and to
; compute the RMS variation. Then copy the results into the unused data
; field named "Fltrd".
; also strips the Albedo by band and BB TOA fields in the data table as well
; as Refl and Temp fields
;---------------------------------------------------------------------------


bad_hts = -9999

heights_in = (WC_OR_ZW EQ 'WC') ? RowData.wind_ht : $
                                  RowData.zero_ht
   
SmoothRetrievedValues1, RowData.deg_lon, RowData.deg_lat, RowData.terr_ht, $
                        RowData.zero_ht, heights_in, bad_hts, heights_out, $
                        NdxsGood, RMS_dev, Status

num_table_lines = EndLine - BegLine
remove1_start = 104
cut1_len = 36
cut_end_start = 243
FOR irow=0,num_table_lines-1 DO BEGIN
   row_ndx = irow + BegLine
   line_data = FileArray[row_ndx]
   STRPUT, line_data, STRING(FORMAT='(I5)', heights_out[irow]), 77
   line_data = STRMID(line_data,0,cut_end_start)
   before_cut1 = STRMID(line_data,0,remove1_start)
   after_cut1 = STRMID(line_data,remove1_start+cut1_len)
   line_data = before_cut1 + after_cut1
   FileArray[row_ndx] = line_data
ENDFOR

;---------------------------------------------------------------------------
; Compute the standard deviation, best top height and best median height.
;---------------------------------------------------------------------------

;DLNDLN - fix below in MINX also ??

NumGoodHts = N_ELEMENTS(NdxsGood)
IF (NumGoodHts EQ 1 AND NdxsGood[0] LT 0) THEN NumGoodHts = 0

IF (NumGoodHts GE 2) THEN BEGIN
   Std_Dev = StdDev(heights_out[NdxsGood])

   GetBestHeights1, RowData.deg_lon, RowData.deg_lat, heights_in, $
                    heights_out, MeanHt, MedHt, MaxHt
ENDIF ELSE BEGIN
   Std_Dev = -9999.0
   IF (NumGoodHts EQ 1) THEN BEGIN
      MeanHt = ROUND(heights_out[NdxsGood])
      MedHt  = MeanHt
      MaxHt  = MeanHt
   ENDIF ELSE BEGIN
      MeanHt = -9999
      MedHt  = -9999
      MaxHt  = -9999
   ENDELSE
ENDELSE

heights_in = 0
heights_out = 0
NdxsGood = 0

END  ;  ComputeTableValues

;***************************************************************************
PRO AccumDataFields, LineNum, NewLine, FieldData
;***************************************************************************
; Save the values in the data table for certain parameters.
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Hardwire the beg column and byte length of each field.
;---------------------------------------------------------------------------

ColBeg = [57, 63, 70, 77, 200]
ColLen = [ 4,  5,  5,  5,   6]

;---------------------------------------------------------------------------
; Get the values and stuff them in the data structure.
;---------------------------------------------------------------------------

FieldData.terr_ht[LineNum] = FIX(STRMID(NewLine,   ColBeg[0], ColLen[0]))
FieldData.zero_ht[LineNum] = FIX(STRMID(NewLine,   ColBeg[1], ColLen[1]))
FieldData.wind_ht[LineNum] = FIX(STRMID(NewLine,   ColBeg[2], ColLen[2]))
FieldData.fltr_ht[LineNum] = FIX(STRMID(NewLine,   ColBeg[3], ColLen[3]))
FieldData.fire_RP[LineNum] = FLOAT(STRMID(NewLine, ColBeg[4], ColLen[4]))

END  ;  AccumDataFields

;***************************************************************************
PRO Update_SmokePlumeFiles_OCF
;***************************************************************************
; Loop over all MINX raw smoke plume text files for old OCF based projects
; and change numerous items in the headers and in the data point list:
;  0) Change MINX version to V4.0.
;  1) Change 'Region name' value so names have consistent length:
;     orbit number must have 6 characters;
;     block number must have 3 characters;
;     plume number in each block must have 2 characters.
;  2) Save value of 'Region wind dir type   :'
;  3) Save value of 'Retrieved with band    :'
;  4) Rename 'Images in "true color" :' to 'Image color equalized  :'.
;  5) Rename 'Images in "Equalized color" :' to  'Image color equalized  :'.
;  6) Change value of 'First point longitude  :' to fire location if avail.
;  7) Change value of 'First point latitude   :' to fire location if avail.
;  7a) Insert a line for 'Geographic region      :'.
;  7b) Insert a line for 'Biome IGBP name, class :'.
;  7c) Insert a line for 'Red/blue band better?  :'
;  8) Save value of 'Area (sq km)           :'.
;  9) Save value of 'Area per point (sq km) :'.
; 10) Save value of 'Percent area covered   :'.
; 11) Rename 'Best median ht (m ASL) :' to 'Median ht (m > ground) :'
; 11a) Change 'Median ht (m > ground)    :' value to new estimate and
;     reference to ground level.
; 12) Rename 'Best top ht (m ASL)    :' to 'Max ht (m > ground)    :'
; 12a) Change 'Max ht (m > ground)    :' value to new estimate and
;     reference to ground level.
; 13) Rename 'StdDev metric, corrht  :' to 'Ht std deviation (m)   :'.
; 14) Rename 'StdDev metric, zeroht  :' to 'Ht std deviation (m)   :'.
; 14a) Insert a line for 'Ht local variation (m) :'.
; 15) Rename 'Auto retrieval quality :' to 'Retrieval quality est. :'.
; 15a) Change 'Retrieval quality est. :' value to new one.
; 16) Remove 'User retrieval quality :'.
; 17) Insert a line at end of header part 2 for 'IGBP biome grid file   :'.
; 18) In the Results table:
;   - Save all lons, lats and heights.
;   - Compute and save the image coordinates for all points.
;   - Populate the field 'Fltrd' with new heights from the Delaunay
;     smoothing algorithm.
;   - Compute the best max and median hts and RMS deviation.
;   - In "cloud" files, change all values of 0 for wind-corr height to -9999,
;     and change all values of 0.0 for winds to -99.9.
;   - Find the plume pixel where the largest fire power was recorded and
;     save its lat/lon coordinates to serve as the location of the fire for
;     identifying the geographic region and biome.
;   - Compute the automatic quality flag value and write it in.
; 19) Update the plume file names to have consistent character length.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;***************************************************************************
;DLNDLN - These must be changed for each project to be processed.

;months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
;          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

;mnth = ''
;PRINT, 'Enter the month to process (e.g. Jan)'
;READ, mnth
       
;project_base_dir = '/Users/dlnelson/00_Atemp/'
;rating_dflt_file = 'PlumeRating.txt'

;project_base_dir = '/Users/dlnelson/00_BestHtTest/'
;rating_dflt_file = 'PlumeRating.txt'

project_base_dir = '/Users/sval/Downloads/MINX_AMAZON2010/MINX/'

;project_base_dir = '/data/plume/WebResults/Global2008/' + mnth + '/'
;rating_dflt_file = 'PlumeRating_' + mnth + '.txt'

biome_dflt_dir = '/Users/sval/Downloads/'
biome_dflt_file = 'MODIS_MCD12C1_IGBP_BiomeGrid_2008.dat'

verification_file = 'UpdatePlumeFiles_ErrorLog.txt'
;***************************************************************************

NUM_OLD_HDR_LINES = 40     ; this must be correct - up to POLYGON:
NUM_NEW_HDR_LINES = 51     ; this must be correct - up to POLYGON:
NUM_AFFECTED_ENTRIES = 29  ; this must be correct

LABEL_LENGTH = [16, 24]
NUM_CHAR_IN_RESULTS = 206.0
APPRX_CHARS_IN_HEADER_LINES = 40
APPRX_NUMBER_PREPT_LINES = 72.0
BEG_COLUMN_FOR_FLTRD = 77
POS_ZEROHT = 63  ; zero-based column # in table where zero-wind ht should be
POS_WINDHT = 70  ; zero-based column # in table where wind-corr ht should be
POS_FLTRHT = 77  ; zero-based column # in table where smoothed ht should be

buff = ''
num_analyze_errors = 0
num_verify_errors = 0
num_filename_errors = 0
num_lines_more_in_new = NUM_NEW_HDR_LINES - NUM_OLD_HDR_LINES

end_header_text = 'POLYGON:'
len_end_header_text = STRLEN(end_header_text)

begin_table_text = 'RESULTS:'
len_begin_table_text = STRLEN(begin_table_text)

;---------------------------------------------------------------------------
; Establish where modifications will be made and what the label replacements
; are if any.
;---------------------------------------------------------------------------
; OLD : Match blue in An only? : No
; NEW : Min ht > terrain (km)  :  0.250 -> -9.999
; NEW : Max ht > sealevel (km) :  6.000 -> -9.999
; NEW : Sample spacing (km)    :  0.550 -> -9.999
; NEW : Registration corrected : Yes -> NA
; NEW : Plume has pyro-cumulus :  No -> NA
; NEW : Comments by digitizer  :  medium, isolated -> None 
; CHG : Retrieval quality      :  FAIR -> Retrieval quality est.

; DATA TABLE HEADER OLD
;            Long-    Lat-                   Km to  Dg Cw  Terr    Feature Ht (m)      Windspeed (m/s)          Albedo by Band        BB TOA     Optical Depth by Band      Single-Scattering Albedo     Tau Fraction by Particle Type   Ang   Power  Refl   BT21   BT31   BBT21  BBT31
;     Pt#    itude    itude   Blk Samp Line   Pt 1  Rel N  Elev  NoWnd  W/Wnd  Fltrd  Across Along Total  Blue   Green   Red    NIR   Albedo   Blue   Green   Red    NIR    Blue   Green   Red    NIR    Small  Medium   Large   Spher   Exp   MWatt  0->1   Deg K  Deg K  Deg K  Deg K
;  ------  --------  -------  --- ---- ----  -----  -----  ----  -------------------  ------------------  --------------------------  ------  ---------------------------  ---------------------------  ------------------------------  -----  -----  ----   -----  -----  -----  -----

; DATA TABLE HEADER NEW
;            Long-    Lat-                   Km to  Dg Cw  Terr    Feature Ht (m)      Windspeed (m/s)       Optical Depth by Band      Single-Scattering Albedo     Tau Fraction by Particle Type   Ang   Power
;     Pt#    itude    itude   Blk Samp Line   Pt 1  Rel N  Elev  NoWnd  W/Wnd  Fltrd  Across Along Total   Blue   Green   Red    NIR    Blue   Green   Red    NIR    Small  Medium   Large   Spher   Exp   MWatt
;  ------  --------  -------  --- ---- ----  -----  -----  ----  -------------------  ------------------  ---------------------------  ---------------------------  ------------------------------  -----  -----
OldLabelList = $
  ['MINX version   :',         'Region name            :', 'Region wind dir type   :', $
   'Retrieved with band    :', 'Images in "true color" :', 'Images in "Equalized col', $
   'First point longitude  :', 'First point latitude   :', 'Area (sq km)           :', $
   'Area per point (sq km) :', 'Percent area covered   :', 'Best median ht (m ASL) :', $
   'Best top ht (m ASL)    :', 'StdDev metric, corrht  :', 'StdDev metric, zeroht  :', $
   'Retrieval quality      :', 'User retrieval quality :', 'Aerosol product file   :', $
   'Wind-corrected points  :', 'Zero-wind points       :', '|WndDir-AlongDir|(deg) :', $
   'Power of fire in MW    :', 'Match blue in An only? :', 'Retrieval precision    :']

NewLabelList = $
  ['MINX version   :',         'Region name            :', 'Region wind dir type   :', $
   'Retrieved with band    :', 'Image color equalized  :', 'Image color equalized  :', $
   'First point longitude  :', 'First point latitude   :', 'Area (sq km)           :', $
   'Area per point (sq km) :', 'Percent area covered   :', 'Median ht (m > fire)   :', $
   'Max ht (m > fire)      :', 'Ht std. deviation (m)  :', 'Ht std. deviation (m)  :', $
   'Retrieval quality est. :', 'REMOVE                 :', 'IGBP biome grid file   :', $
   'Num. heights retrieved :', 'Num. heights retrieved :', '|WndDir-AlongDir|(deg) :', $
   'Total fire power (MW)  :', 'REMOVE                 :', 'Retrieval precision    :']

AddedLabelList = $ 
  ['Geographic region      :', 'Biome IGBP name, class :', 'Red/blue band better?  :', $
   'Ht local variation (m) :', 'IGBP biome grid file   :', 'Fire elev. (m > MSL)   :', $
   'Min ht > terrain (km)  :', 'Max ht > sealevel (km) :', 'Sample spacing (km)    :', $
   'Registration corrected :', 'Plume has pyro-cumulus :', 'Comments by digitizer  :']

;---------------------------------------------------------------------------
; Set the 0-based field numbers in the RESULTS: table for certain fields we
; want to process. All fields are separated by at least one space character.
;---------------------------------------------------------------------------

field_ndx = [1, 2, 8, 9, 10, 11, 28]

;---------------------------------------------------------------------------
; Get the project directory for which the text files will be updated.
; Create a new project directory to contain all the files after they have
; had new information inserted and their file names updated.
;---------------------------------------------------------------------------

retry1:
PLUME_old_proj_dir = $
   DIALOG_PICKFILE(TITLE='Select directory containing project to update', $
                   PATH=project_base_dir, FILE=project_base_dir, $
                   GET_PATH=proj_path, /DIRECTORY)
IF (PLUME_old_proj_dir EQ '') THEN RETURN

ilen = STRLEN(PLUME_old_proj_dir)
PLUME_old_proj_dir = STRMID(PLUME_old_proj_dir, 0, ilen-1)

PLUME_new_proj_dir = PLUME_old_proj_dir + '_UPDATED/'
PLUME_old_proj_dir += '/'

;---------------------------------------------------------------------------
; Create an array containing the list of every raw plume .txt file name and
; count the plume files. Red-band and blue-band plumes are separate elements.
; There are 4 blue-band and 7 red-band files per plume.
;---------------------------------------------------------------------------

CD, PLUME_old_proj_dir
PlumeListAry = FILE_SEARCH('0*/*.txt')
num_plumes = N_ELEMENTS(PlumeListAry)

IF (PlumeListAry[0] EQ '' OR num_plumes LT 1) THEN BEGIN
   mssg = ['No plumes were found in directory:', PLUME_old_proj_dir, $
           'Do you want to try again?']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /QUESTION)
   IF (STRUPCASE(rtrn) EQ 'NO') THEN RETURN
   GOTO, retry1
ENDIF

plume_bad_mask = BYTARR(num_plumes)

;---------------------------------------------------------------------------
; Open the error log file. Delete it first.
;---------------------------------------------------------------------------

error_log_filename = PLUME_old_proj_dir + '/' + verification_file
IF (FILE_TEST(error_log_filename)) THEN FILE_DELETE, error_log_filename

OPENW, UnitG, error_log_filename, /GET_LUN

PRINTF, UnitG, 'Beginning Pass to Detect Missed Changes'
PRINTF, UnitG, '---------------------------------------'

;---------------------------------------------------------------------------
; Create the new directory if it doesn't already exist. If it exists, give
; user the opportunity to clear its contents first.
;---------------------------------------------------------------------------

mssg = ['All plume files will be copied and written to a new project', $
        'directory. The raw text files will be updated with new', $
        'information and all files will be renamed to have consistent', $
        'lengths in orbit, block and plume number components.', '', $
        'The new project directory name will be:', PLUME_new_proj_dir, $
        '', 'Is this directory OK? If not this program will end.']
rtrn = DIALOG_MESSAGE(mssg, /CENTER, /QUESTION)

IF (STRUPCASE(rtrn) EQ 'YES') THEN BEGIN
   IF (FILE_TEST(PLUME_new_proj_dir, /DIRECTORY)) THEN BEGIN
      mssg = ['This directory already exists:', PLUME_new_proj_dir, '', $
              'Do you want to empty its contents before proceeding?', $
              '(This could take several minutes.)']
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /QUESTION)
      
      IF (STRUPCASE(rtrn) EQ 'YES') THEN BEGIN
         SPAWN, 'rm -fRv ' + PLUME_new_proj_dir + '*'
      ENDIF
   ENDIF ELSE BEGIN
      FILE_MKDIR, PLUME_new_proj_dir
   ENDELSE
ENDIF ELSE BEGIN
   RETURN
ENDELSE

;***************************************************************************
; Read in the geographic coordinates of global region polygons and construct
; region objects for testing inclusion.
;---------------------------------------------------------------------------

Get_RGN_Polygons1, NumGeoRgn, poly_ptr, rgn_names

rgn_obj = OBJARR(NumGeoRgn)

FOR irgn=0,NumGeoRgn-1 DO $
   rgn_obj[irgn] = Obj_New('IDLanROI', (*(poly_ptr[irgn]))[1,*], $
                                       (*(poly_ptr[irgn]))[0,*])

FOR irgn=0,NumGeoRgn-1 DO PTR_FREE, poly_ptr[irgn]

poly_ptr = 0

;---------------------------------------------------------------------------
; Read in the biome grid containing global biome codes.
;---------------------------------------------------------------------------

Read_MODIS_LandType_Grid, biome_dflt_dir, biome_dflt_file, BiomeGridfile, $
                          BiomeGridSpacing, BiomeGrid, Retval
   

;***************************************************************************
; Process each plume file.
;---------------------------------------------------------------------------

FOR iplume=0,num_plumes-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Get the important, individual components of the full pathname.
   ;------------------------------------------------------------------------
   
   toks0 = STRSPLIT(PlumeListAry[iplume], '/', /EXTRACT, COUNT=numtoks0)
   IF (numtoks0 NE 2) THEN BEGIN
      PRINTF, UnitG, 'Plume ' + PlumeListAry[iplume] + $
              ' has wrong number of components.'
      num_analyze_errors += 1
      CONTINUE
   ENDIF
   
   toks1 = STRSPLIT(toks0[1], '._', /EXTRACT, COUNT=numtoks1)
   IF (numtoks1 NE 3) THEN BEGIN
      PRINTF, UnitG, 'Plume ' + PLUME_old_plume_name + $
              ' has wrong number of components.'
      num_analyze_errors += 1
      CONTINUE
   ENDIF
   
   PLUME_orbit_subdir    = (STRLEN(toks0[0]) EQ 6) ? toks0[0] : '0' + toks0[0]
   PLUME_old_filename    = toks0[1]
   PLUME_old_plume_name  = toks1[1]
   PLUME_old_directory   = PLUME_old_proj_dir  + PLUME_orbit_subdir + '/'
   PLUME_old_entire_path = PLUME_old_directory + PLUME_old_filename
   PLUME_new_directory   = PLUME_new_proj_dir  + PLUME_orbit_subdir + '/'
   PLUME_new_entire_path = PLUME_new_directory + PLUME_old_filename
   
   toks0 = 0 & toks1 = 0
   
   ;------------------------------------------------------------------------
   ; Determine from the file name whether this is a wind-corrected "plume"
   ; where wind direction has been supplied or a non-wind-corrected "cloud".
   ;------------------------------------------------------------------------
   
   wc_or_zw = 'WC'
   toksX = STRSPLIT(PLUME_old_plume_name, '-', /EXTRACT, COUNT=numtoksX)
   IF (numtoksX EQ 3) THEN BEGIN
      wczw = STRMID(toksX[2], 2, 1)
      IF (wczw EQ 'W') THEN wc_or_zw = 'WC'
      IF (wczw EQ 'N') THEN wc_or_zw = 'ZW'
   ENDIF
   toksX = 0

   ;************************************************************************
   ; Copy all the files with the same plume name component as the .txt file
   ; to the new project directory as is.
   ;------------------------------------------------------------------------
   
   CopyFilesForPlumeAsIs, PLUME_old_directory, PLUME_new_directory, $
                          PLUME_old_plume_name
      
   ;------------------------------------------------------------------------
   ; Estimate the number of lines in the file, then double it for safety.
   ; Then allocate a string array large enough to handle the entire file.
   ;------------------------------------------------------------------------
   
   file_struct = FILE_INFO(PLUME_old_entire_path)
   max_lines = (FIX((file_struct.size - $
                APPRX_NUMBER_PREPT_LINES * APPRX_CHARS_IN_HEADER_LINES) / $
                NUM_CHAR_IN_RESULTS) + APPRX_NUMBER_PREPT_LINES) * 2
   file_array = STRARR(max_lines)
   
   ;------------------------------------------------------------------------
   ; Read the entire file into a string array in memory and close the file.
   ; Save the line numbers of the end of header and beginning of data table.
   ; Check if any line contains asterisks - if so report it.
   ;------------------------------------------------------------------------
   
   OPENR, UnitF, PLUME_old_entire_path, /GET_LUN
   
   num_line = 0
   end_header_line = 0
   beg_table_line = 0
   end_table_line = 0
   
   WHILE ~EOF(UnitF) DO BEGIN
      READF, UnitF, buff
      IF (num_line GE max_lines)  THEN BEGIN
         mssg = ['The file line array is not large enough for plume:', $
                 PLUME_old_plume_name, $
                 'Increase the allocation in the code and try again.']
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         FREE_LUN, UnitF
         RETURN
      ENDIF
      
      IF (STRMID(buff, 0, len_end_header_text) EQ end_header_text) THEN $
         end_header_line = num_line
      IF (STRMID(buff, 0, len_begin_table_text) EQ begin_table_text) THEN $
         beg_table_line = num_line

      file_array[num_line] = buff
      num_line += 1
   ENDWHILE
   
   FREE_LUN, UnitF
   
   file_array = file_array[0:num_line + num_lines_more_in_new - 1]
   end_header_line += num_lines_more_in_new
   table_header_line1 = beg_table_line + 1
   table_header_line2 = beg_table_line + 2
   table_header_line3 = beg_table_line + 3   
   beg_table_line += 4
   end_table_line = num_line
   num_table_lines = end_table_line - beg_table_line
   
   ;------------------------------------------------------------------------
   ; Cut the RESULTS table header to exclude Albedo and BB TOA fields
   ; Also cuts end Refl and Temps
   ;------------------------------------------------------------------------
   
   remove_start = 104
   cut_len = 36
   cut_end_start = 243
   header_line1 = file_array[table_header_line1]
   header_line2 = file_array[table_header_line2]
   header_line3 = file_array[table_header_line3]

   header_line1 = STRMID(header_line1,0,cut_end_start)
   before_cut1 = STRMID(header_line1,0,remove_start)
   after_cut1 = STRMID(header_line1,remove_start+cut_len)
   header_line1 = before_cut1 + after_cut1
   file_array[table_header_line1] = header_line1
   
   header_line2 = STRMID(header_line2,0,cut_end_start)
   before_cut2 = STRMID(header_line2,0,remove_start)
   after_cut2 = STRMID(header_line2,remove_start+cut_len)
   header_line2 = before_cut2 + after_cut2
   file_array[table_header_line2] = header_line2
   
   header_line3 = STRMID(header_line3,0,cut_end_start)
   before_cut3 = STRMID(header_line3,0,remove_start)
   after_cut3 = STRMID(header_line3,remove_start+cut_len)
   header_line3 = before_cut3 + after_cut3
   file_array[table_header_line3] = header_line3
   
   ;************************************************************************
   ; Save data from certain fields in the data table RESULTS: in a structure
   ; to use in producing new header (and RESULTS: table) values.
   ;------------------------------------------------------------------------
   
   row_data = { deg_lon : FLTARR(num_table_lines), $
                deg_lat : FLTARR(num_table_lines), $
                terr_ht : INTARR(num_table_lines), $
                wind_ht : INTARR(num_table_lines), $
                zero_ht : INTARR(num_table_lines), $
                fire_RP : FLTARR(num_table_lines) }
   
   total_FRP = 0.0
   total_FRP_str = '-9999'

   FOR irow=0,num_table_lines-1 DO BEGIN
      row_ndx = irow + beg_table_line
      toks = STRSPLIT(file_array[row_ndx], ' ', /EXTRACT, COUNT=numtoks)
      row_data.deg_lon[irow] = FLOAT(toks[field_ndx[0]])
      row_data.deg_lat[irow] = FLOAT(toks[field_ndx[1]])
      row_data.terr_ht[irow] = FIX(toks[field_ndx[2]])

      zero_ht = FIX(toks[field_ndx[3]])
      IF (zero_ht EQ -99) THEN BEGIN
         zero_ht = -9999
         dummy = file_array[row_ndx]
         STRPUT, dummy, '-9999', POS_ZEROHT
         file_array[row_ndx] = dummy
      ENDIF
      row_data.zero_ht[irow] = zero_ht

      wind_ht = FIX(toks[field_ndx[4]])
      IF (wind_ht EQ -99) THEN BEGIN
         wind_ht = -9999
         dummy = file_array[row_ndx]
         STRPUT, dummy, '-9999', POS_WINDHT
         file_array[row_ndx] = dummy
      ENDIF
      row_data.wind_ht[irow] = wind_ht
      
      fltrd_ht = FIX(toks[field_ndx[5]])
      IF (fltrd_ht EQ -99) THEN BEGIN
         fltrd_ht = -9999
         dummy = file_array[row_ndx]
         STRPUT, dummy, '-9999', POS_FLTRHT
         file_array[row_ndx] = dummy
      ENDIF
      
      IF (N_ELEMENTS(toks) LT field_ndx[6] + 1) THEN BEGIN
         row_data.fire_RP[irow] = -99.9
      ENDIF ELSE BEGIN
         row_data.fire_RP[irow] = FLOAT(toks[field_ndx[6]])
         IF (row_data.fire_RP[irow] GT 0.0) THEN $
            total_FRP += row_data.fire_RP[irow]
      ENDELSE
   ENDFOR
 
   IF (total_FRP GT 0.0 AND wc_or_zw EQ 'WC') THEN $
      total_FRP_str = STRTRIM(STRING(ROUND(total_FRP)),2)

   ;************************************************************************
   ; Find the lon and lat of the fire pixel with largest radiative fire
   ; power. If there is no fire pixel or if this is a non-wind-corrected
   ; smoke "cloud", then use the location of the first point digitized. Also
   ; use this location as the location where the geographic region and biome
   ; type are recorded.
   ;------------------------------------------------------------------------
   
   max_fr_power = 0.0
   biome_lon = row_data.deg_lon[0]
   biome_lat = row_data.deg_lat[0]
   fire_height = -9999
   
   IF (wc_or_zw EQ 'WC') THEN BEGIN
      FOR irow=0,num_table_lines-1 DO BEGIN
         IF (row_data.fire_RP[irow] GT max_fr_power) THEN BEGIN
            max_fr_power = row_data.fire_RP[irow]
            biome_lon = row_data.deg_lon[irow]
            biome_lat = row_data.deg_lat[irow]
            fire_height = row_data.terr_ht[irow]
         ENDIF
      ENDFOR
   ENDIF
   
   IF (fire_height EQ -9999) THEN fire_height = row_data.terr_ht[0]

   ;------------------------------------------------------------------------
   ; Find the geographic region at the fire location and add the value to
   ; the new text inserted above.
   ;------------------------------------------------------------------------
   
   geo_region_name = 'NA'

   FOR irgn=0,NumGeoRgn-1 DO BEGIN
      IF (rgn_obj[irgn]->ContainsPoints([biome_lon, biome_lat])) THEN BEGIN
         geo_region_name = rgn_names[irgn]
         BREAK
      ENDIF
   ENDFOR
   
   ;------------------------------------------------------------------------
   ; Find the biome code and name at the fire location and add the value to
   ; the new text inserted above.
   ;------------------------------------------------------------------------
   
   Get_BiomeType_At_LatLong, BiomeGrid, BiomeGridSpacing, biome_lon, $
                             biome_lat, biome_code, biome_name
      
   biome_and_code = biome_name + ', ' + STRTRIM(STRING(FIX(biome_code)),2)
   
   ;************************************************************************
   ; Old code only had red band retrievals, so set that to the best band.
   ;------------------------------------------------------------------------
   
   ; better_band = 'NA'
   better_band = 'Red'
   

   
   ;************************************************************************
   ; Loop over the header lines as many times as there are lines to be added
   ; or removed or changed.
   ;------------------------------------------------------------------------
   
   num_labl_mod = 0
   num_data_mod = 0
   num_data_sav = 0
   num_line_add = 0
   num_line_del = 0

   RMS_dev = 0.0
   num_good_hts = 0
   plume_area = 0.0
   area_per_pt = 0.0
   ihdr_new = 0

   FOR imod=0,NUM_AFFECTED_ENTRIES DO BEGIN
   
      FOR ihdr=ihdr_new,end_header_line DO BEGIN
      
         ;------------------------------------------------------------------
         ; Get the label and value for the next line in the header.
         ;------------------------------------------------------------------
      
         llength = (ihdr_new LE 8) ? LABEL_LENGTH[0] : LABEL_LENGTH[1]
         FileLabel = STRMID(file_array[ihdr_new], 0, llength)
         LabelValue = STRTRIM(STRMID(file_array[ihdr_new], llength), 2)
         IF (FileLabel EQ OldLabelList[5]) THEN $
            LabelValue = STRTRIM(STRMID(file_array[ihdr_new], llength+6), 2)

         ;------------------------------------------------------------------
         ; If the lines match, process the changes.
         ;------------------------------------------------------------------

         CASE FileLabel OF
         
            OldLabelList[0]: BEGIN ; MINX version
               file_array[ihdr_new] = NewLabelList[0] + ' V4.0'
               num_data_mod += 1
            END
          
            OldLabelList[1]: BEGIN ; region name
               UpdatePlumeName, PLUME_old_plume_name, NewPlumeName
               file_array[ihdr_new] = NewLabelList[1] + ' ' + NewPlumeName
               num_data_mod += 1
            END
         
            OldLabelList[2]: BEGIN ; wind dir type
               ComputeTableValues, wc_or_zw, file_array, row_data, $
                                   beg_table_line, end_table_line, RMS_dev, $
                                   num_good_hts, mean_fltr_ht, med_fltr_ht, $
                                   max_fltr_ht, std_dev
               num_data_sav += 1
            END
      
            OldLabelList[3]: BEGIN ; retrieved with band
               file_array[ihdr_new] = NewLabelList[3] + ' ' + LabelValue
               num_data_sav += 1
            END
            
            OldLabelList[4]: BEGIN ; in true color?
               file_array[ihdr_new] = NewLabelList[4] + ' ' + LabelValue
               num_labl_mod += 1
            END
            
            OldLabelList[5]: BEGIN ; in "equalized color?
               file_array[ihdr_new] = NewLabelList[5] + ' ' + LabelValue
               num_labl_mod += 1
            END
            
            OldLabelList[6]: BEGIN ; 1st point longitude
               file_array[ihdr_new] = NewLabelList[6] + '  ' + $
                                    STRING(FORMAT='(F8.3)', biome_lon)
               num_data_mod += 1
            END
            
            OldLabelList[7]: BEGIN ; 1st point latitude
               file_array[ihdr_new] = NewLabelList[7] + '  ' + $
                                    STRING(FORMAT='(F8.3)', biome_lat)
               num_data_mod += 1
               new_line = AddedLabelList[0] + ' ' + geo_region_name
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[1] + ' ' + biome_and_code
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[2] + ' ' + better_band
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
            END
            
            OldLabelList[8]: BEGIN ; region area
               plume_area = FLOAT(LabelValue)
               num_data_sav += 1
            END
            
            OldLabelList[9]: BEGIN ; area per point
               area_per_pt = FLOAT(LabelValue)
               num_data_sav += 1
            END
            
            OldLabelList[10]: BEGIN ; % region covered
               fraction_area = ((num_good_hts*area_per_pt)/plume_area) < 1.0
               file_array[ihdr_new] = NewLabelList[10] + ' ' + $
                           STRING(FORMAT='(I5)', ROUND(fraction_area * 100.0))
               num_data_mod += 1
               new_line = AddedLabelList[5] + ' ' + STRING(FORMAT='(I5)', $
                          fire_height)
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
            END
            
            OldLabelList[11]: BEGIN ; median height
               val1 = (med_fltr_ht EQ -9999. OR fire_height EQ -9999) ? $
                       med_fltr_ht : med_fltr_ht - fire_height
               file_array[ihdr_new] = NewLabelList[11] + ' ' + $
                       STRING(FORMAT='(I5)', val1)
               num_labl_mod += 1
               num_data_mod += 1
            END

            OldLabelList[12]: BEGIN ; max height
               val1 = (max_fltr_ht EQ -9999. OR fire_height EQ -9999) ? $
                       max_fltr_ht : max_fltr_ht - fire_height
               save_max_ht = val1
               file_array[ihdr_new] = NewLabelList[12] + ' ' + $
                          STRING(FORMAT='(I5)', val1)
               num_labl_mod += 1
               num_data_mod += 1
             END
            
            OldLabelList[13]: BEGIN ; local variation & std dev, corrht
               file_array[ihdr_new] = NewLabelList[13] + ' ' + $
                                 STRING(FORMAT='(I5)', ROUND(std_dev))
               num_labl_mod += 1
               num_data_mod += 1

               new_line = AddedLabelList[3] + ' ' + STRING(FORMAT='(I5)', $
                          ROUND(RMS_dev))
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
            END
            
            OldLabelList[14]: BEGIN ; local variation & std dev, zeroht
               file_array[ihdr_new] = NewLabelList[14] + ' ' + $
                                 STRING(FORMAT='(I5)', ROUND(std_dev))
               num_labl_mod += 1
               num_data_mod += 1

               new_line = AddedLabelList[3] + ' ' + STRING(FORMAT='(I5)', $
                          ROUND(RMS_dev))
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
            END
            
            OldLabelList[15]: BEGIN ; retrieval quality
               ndxs1 = WHERE(row_data.terr_ht NE -99, numndxs1)
               IF (numndxs1 GT 0) THEN mean_hts = (mean_fltr_ht - $
                            MEAN(row_data.terr_ht[ndxs1])) / 1000.0
               ComputePlumeQuality1, num_good_hts, fraction_area, $
                           RMS_dev / 1000.0, save_max_ht / 1000.0, $
                                     auto_quality
               file_array[ihdr_new] = NewLabelList[15] + '  ' + $
                                      auto_quality
               num_labl_mod += 1
               num_data_mod += 1
               ; Plume has pyro-cumulus :  NA
               ; Comments by digitizer  :  None
               new_line = AddedLabelList[10] + ' ' + 'NA'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[11] + ' ' + 'None'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1               
            END
            
            ; OCF version files do not have this field
            ; OldLabelList[16]: BEGIN ; user retrieval quality
            ;    DeleteEntryInArray, file_array, ihdr_new
            ;    num_line_del += 1
            ;    ihdr_new -= 1
            ; END
            
            OldLabelList[17]: BEGIN ; biome file name
               new_line = AddedLabelList[4] + ' ' + biome_dflt_file
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1            
               ihdr_new += 1
            END
            
            OldLabelList[18]: BEGIN ; Zero-wind points
               file_array[ihdr_new] = NewLabelList[18] + ' ' + $
                    STRING(FORMAT='(I5)', num_good_hts) ; LabelValue)
;               num_good_hts = FLOAT(LabelValue)
               num_labl_mod += 1
               num_data_sav += 1
            END
            
            OldLabelList[19]: BEGIN ; Wind-corrected points
               file_array[ihdr_new] = NewLabelList[19] + ' ' + $
                    STRING(FORMAT='(I5)',num_good_hts) ; LabelValue)
;               num_good_hts = FLOAT(LabelValue)
               num_labl_mod += 1
               num_data_sav += 1
            END
            
            OldLabelList[20]: BEGIN ; wind direction
               IF (LabelValue EQ 'NA') THEN $
                  file_array[ihdr_new] = NewLabelList[20] + ' ' + $
                       STRING(FORMAT='(I5)', -9999)
            END
            
            OldLabelList[21]: BEGIN ; fire power
               file_array[ihdr_new] = NewLabelList[21] + ' ' + $
                       STRING(FORMAT='(I5)', total_FRP_str)
               num_labl_mod += 1
            END
            
            OldLabelList[22]: BEGIN ; match blue in AN only
               DeleteEntryInArray, file_array, ihdr_new
               num_line_del += 1
               ihdr_new -= 1
            END            
            
            OldLabelList[23]: BEGIN ; Retrieval precision
               file_array[ihdr_new] = NewLabelList[23] + ' ' + LabelValue
               ; Min ht > terrain (km)  :  -9.999
               ; Max ht > sealevel (km) :  -9.999
               ; Sample spacing (km)    :  -9.999
               ; Registration corrected : NA               
               new_line = AddedLabelList[6] + ' ' + '-9.999'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[7] + ' ' + '-9.999'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[8] + ' ' + '-9.999'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
               new_line = AddedLabelList[9] + ' ' + 'NA'
               InsertEntryInArray, file_array, ihdr_new, new_line
               num_line_add += 1
               ihdr_new += 1
            END
            
            
            ELSE:
         ENDCASE

         ihdr_new += 1

      ENDFOR
   ENDFOR

   ;---------------------------------------------------------------------
   ; Test if we found all the needed header lines.
   ;---------------------------------------------------------------------
   
   IF (num_data_sav NE 5) THEN BEGIN
      num_lin = STRTRIM(STRING(5-num_data_sav),2)
      PRINTF, UnitG, num_lin + ' header line(s) for saving data not ' + $
              'found in ' + PLUME_old_entire_path
      num_analyze_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   IF (num_labl_mod NE 7) THEN BEGIN
      num_lin = STRTRIM(STRING(7-num_labl_mod),2)
      PRINTF, UnitG, num_lin + ' header line(s) for text modification ' + $
              'not found in ' + PLUME_old_entire_path
      num_analyze_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   IF (num_data_mod NE 9) THEN BEGIN
      num_lin = STRTRIM(STRING(9-num_data_mod),2)
      PRINTF, UnitG, num_lin + ' header line(s) for data modification ' + $
              'not found in ' + PLUME_old_entire_path
      num_analyze_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   IF (num_line_add NE 12) THEN BEGIN
      num_lin = STRTRIM(STRING(num_line_add),2)
      PRINTF, UnitG, 'Added ' + num_lin + ' header lines not 12 in ' + $
              PLUME_old_entire_path
      num_analyze_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   IF (num_line_del NE 1) THEN BEGIN
      num_lin = STRTRIM(STRING(1-num_line_del),2)
      PRINTF, UnitG, 'Deleted ' + num_lin + ' header lines not 1 in ' + $
              PLUME_old_entire_path
      num_analyze_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   
   beg_table_line += num_line_add - num_line_del
   end_table_line += num_line_add - num_line_del
   
   ;------------------------------------------------------------------------
   ; Open a new file in the new directory, write the modified lines out to
   ; the new file and close the file.
   ;------------------------------------------------------------------------
   
   OPENW, UnitF, PLUME_new_entire_path, /GET_LUN
   
   FOR iline=0,end_table_line-1 DO PRINTF, UnitF, file_array[iline]
   
   FREE_LUN, UnitF
   
   file_struct = 0
   file_array  = 0
   
   ;------------------------------------------------------------------------
   ; Rename plume files with the current plume name component so they have
   ; the correct constant length format.
   ;------------------------------------------------------------------------
   
   RenamePlumeFilesForOrbit, PLUME_new_directory, PLUME_old_filename, $
                             PLUME_old_plume_name, num_files_renamed, retval
ENDFOR

FOR irgn=0,NumGeoRgn-1 DO OBJ_DESTROY, rgn_obj[irgn]
toks = 0
field_ndx = 0
rgn_obj = 0
BiomeGrid = 0
rgn_names = 0
row_data.deg_lon = 0
row_data.deg_lat = 0
row_data.terr_ht = 0
row_data.wind_ht = 0
row_data.zero_ht = 0
row_data.fire_RP = 0
row_data = 0

PRINTF, UnitG, (num_analyze_errors EQ 0) ? '   No analyze errors found.' : $
   '   ' + STRTRIM(STRING(num_analyze_errors),2) + $
   ' analyze errors were found.'

;***************************************************************************
; Now validate the updates by reading in the corresponding .txt files and
; comparing them.
;***************************************************************************

CD, PLUME_new_proj_dir

PRINTF, UnitG, ''
PRINTF, UnitG, 'Beginning Pass to Validate File Contents'
PRINTF, UnitG, '----------------------------------------'

FOR iplume=0,num_plumes-1 DO BEGIN

   FieldData = 0

   ;---------------------------------------------------------------------
   ; Get the important, individual components of the full pathname.
   ;---------------------------------------------------------------------
   
   toks0 = STRSPLIT(PlumeListAry[iplume], '/', /EXTRACT, COUNT=numtoks0)
   toks1 = STRSPLIT(toks0[1], '._', /EXTRACT, COUNT=numtoks1)
   UpdatePlumeName, toks1[1], new_plume_name
   orbit_subdir = (STRLEN(toks0[0]) EQ 6) ? toks0[0] : '0' + toks0[0]
   old_path = PLUME_old_proj_dir + orbit_subdir + '/' + toks0[1]
   new_path = PLUME_new_proj_dir + orbit_subdir + '/Plumes_' + $
              new_plume_name + '.txt'
      
   ;------------------------------------------------------------------------
   ; Estimate the number of lines in the file, then double it for safety.
   ; Then allocate string arrays large enough to handle entire files.
   ;------------------------------------------------------------------------
   
   file_struct = FILE_INFO(new_path)
   max_lines = (FIX((file_struct.size - $
               APPRX_NUMBER_PREPT_LINES * APPRX_CHARS_IN_HEADER_LINES) / $
               NUM_CHAR_IN_RESULTS) + APPRX_NUMBER_PREPT_LINES) * 2
   file_array_old = STRARR(max_lines)
   file_array_new = STRARR(max_lines)
   
   ;---------------------------------------------------------------------
   ; Open the old .txt file and read the entire contents.
   ;---------------------------------------------------------------------
   
   OPENR, UnitOld, old_path, /GET_LUN
   num_lines_old = 0
   WHILE ~EOF(UnitOld) DO BEGIN
      READF, UnitOld, buff
      file_array_old[num_lines_old] = buff
      num_lines_old += 1
   ENDWHILE
   FREE_LUN, UnitOld
   
   ;---------------------------------------------------------------------
   ; Open the new .txt file and read the entire contents.
   ;---------------------------------------------------------------------
   
   OPENR, UnitNew, new_path, /GET_LUN
   num_lines_new = 0
   WHILE ~EOF(UnitNew) DO BEGIN
      READF, UnitNew, buff
      file_array_new[num_lines_new] = buff
      IF (STRMID(buff, 0, 8) EQ 'RESULTS:') THEN $
         table_beg_row = num_lines_new + 4
      num_lines_new += 1
   ENDWHILE
   FREE_LUN, UnitNew
   table_num_row = num_lines_new - table_beg_row
   
   ;---------------------------------------------------------------------
   ; Compare the file contents.
   ;---------------------------------------------------------------------
   
   next_old_line = 0
   next_new_line = 0
   diff_header_count = 0
   diff_body_count = 0
   same_body_count = 0
   
   save_MINX_ver_old = ''
   save_MINX_ver_new = ''
   save_zero_or_wind = ''
   save_first_lon_old = ''
   save_first_lon_new = ''
   save_first_lat_old = ''
   save_first_lat_new = ''
   save_num_pts_old = 0
   save_num_pts_new = 0
   save_pcnt_cover_old = 0
   save_pcnt_cover_new = 0
   save_median_ht_old = 0
   save_median_ht_new = 0
   save_max_ht_old = 0
   save_max_ht_new = 0
   save_locvar_ht = 0
   save_stddev_ht_old = ''
   save_stddev_ht_new = ''
   save_aquality_old = ''
   save_aquality_new = ''
   save_uquality_old = ''
   save_uquality_new = ''
   
   FieldData = { terr_ht : INTARR(table_num_row), $
                 zero_ht : INTARR(table_num_row), $
                 wind_ht : INTARR(table_num_row), $
                 fltr_ht : INTARR(table_num_row), $
                 fire_RP : FLTARR(table_num_row) }

   WHILE (next_new_line LT num_lines_new) DO BEGIN
      old_line = file_array_old[next_old_line]
      new_line = file_array_new[next_new_line]
      
      IF (STRPOS(new_line, '*') GE 0) THEN $
         PRINTF, UnitG, 'Asterisks found in plume: ' + new_path
      IF (next_new_line LT 45 AND STRPOS(new_line, '--') GE 0) THEN $
         PRINTF, UnitG, 'Dashes found in plume: ' + new_path

      toks_old = STRSPLIT(old_line, ':', /EXTRACT, COUNT=numtoks_old)
      toks_old[0] += ':'
      toks_new = STRSPLIT(new_line, ':', /EXTRACT, COUNT=numtoks_new)
      toks_new[0] += ':'

      IF (toks_old[0] EQ OldLabelList[0]) THEN $ ; MINX version
         save_MINX_ver_old = toks_old[1]
      IF (toks_new[0] EQ NewLabelList[0]) THEN $ ; MINX version
         save_MINX_ver_new = toks_new[1]
      IF (toks_new[0] EQ NewLabelList[2]) THEN $ ; wind dir type
         save_zero_or_wind = (STRTRIM(toks_new[1],2) EQ $
         'Direction provided') ? 'wind' : 'zero'
      IF (toks_old[0] EQ OldLabelList[6]) THEN $ ; longitude
         save_first_lon_old = toks_old[1]
      IF (toks_new[0] EQ NewLabelList[6]) THEN $ ; longitude
         save_first_lon_new = toks_new[1]
      IF (toks_old[0] EQ OldLabelList[7]) THEN $ ; latitude
         save_first_lat_old = toks_old[1]
      IF (toks_new[0] EQ NewLabelList[7]) THEN $ ; latitude
         save_first_lat_new = toks_new[1]
      IF (toks_old[0] EQ OldLabelList[18]) THEN $ ; number of pts
         save_num_pts_old = FIX(toks_old[1])
      IF (toks_old[0] EQ OldLabelList[19]) THEN $ ; number of pts
         save_num_pts_old = FIX(toks_old[1])
      IF (toks_new[0] EQ NewLabelList[18]) THEN $ ; number of pts
         save_num_pts_new =FIX(toks_new[1])
      IF (toks_old[0] EQ OldLabelList[10]) THEN $ ; Percent area
         save_pcnt_cover_old = FIX(toks_old[1]) < 100
      IF (toks_new[0] EQ NewLabelList[10]) THEN $ ; Percent area
         save_pcnt_cover_new = FIX(toks_new[1])
      IF (toks_old[0] EQ OldLabelList[11]) THEN $ ; median ht
         save_median_ht_old = (STRPOS(toks_old[1], '*') GE 0) ? $
            -9998 : FIX(toks_old[1])
      IF (toks_new[0] EQ NewLabelList[11]) THEN $ ; Median ht
         save_median_ht_new = (STRPOS(toks_new[1], '*') GE 0) ? $
            -9998 : FIX(toks_new[1])
      IF (toks_old[0] EQ OldLabelList[12]) THEN $ ; top ht
         save_max_ht_old = (STRPOS(toks_old[1], '*') GE 0) ? $
            -9998 : FIX(toks_old[1])
      IF (toks_new[0] EQ NewLabelList[12]) THEN $ ; Max ht
         save_max_ht_new = (STRPOS(toks_new[1], '*') GE 0) ? $
            -9998 : FIX(toks_new[1])
      IF (toks_new[0] EQ AddedLabelList[3]) THEN $ ; local variation
         save_locvar_ht = (STRPOS(toks_new[1], '*') GE 0) ? $
            -9998 : FIX(toks_new[1])
      IF (toks_old[0] EQ OldLabelList[13]) THEN $ ; StdDev metric
         save_stddev_ht_old = toks_old[1]
      IF (toks_old[0] EQ OldLabelList[14]) THEN $ ; StdDev metric
         save_stddev_ht_old = toks_old[1]
      IF (toks_new[0] EQ NewLabelList[13]) THEN $ ; Ht std dev
         save_stddev_ht_new = toks_new[1]
      IF (toks_old[0] EQ OldLabelList[15]) THEN $ ; Auto qual
         save_aquality_old = toks_old[1]
      IF (toks_new[0] EQ NewLabelList[15]) THEN $ ; Retrieval qual
         save_aquality_new = toks_new[1]
      IF (toks_old[0] EQ OldLabelList[16]) THEN $ ; User qual
         save_uquality_old = toks_old[1]
      IF (toks_new[0] EQ OldLabelList[16]) THEN $ ; User qual
         save_uquality_new = toks_new[1]

      IF (old_line NE new_line) THEN BEGIN
         IF (next_new_line LT NUM_NEW_HDR_LINES) THEN BEGIN
         
            ; Skip blank lines.
            IF (numtoks_new LT 2) THEN BEGIN
               IF (numtoks_old LT 2) THEN CONTINUE
               next_new_line += 1
               CONTINUE
            ENDIF ELSE BEGIN
               IF (numtoks_old LT 2) THEN BEGIN
                  next_old_line += 1
                  CONTINUE
               ENDIF
            ENDELSE
            
            ; Test lines where the description has been changed.
            IF ((toks_new[0] EQ NewLabelList[4]  OR toks_new[0] EQ NewLabelList[5]  OR $
                 toks_new[0] EQ NewLabelList[11] OR toks_new[0] EQ NewLabelList[12] OR $
                 toks_new[0] EQ NewLabelList[13] OR toks_new[0] EQ NewLabelList[14] OR $
                 toks_new[0] EQ NewLabelList[15] OR toks_new[0] EQ NewLabelList[18] OR $
                 toks_new[0] EQ NewLabelList[19] OR toks_new[0] EQ NewLabelList[21]) AND $
                 toks_new[0] NE toks_old[0]) THEN $
               diff_header_count += 1

            ; Test lines where the data value has been changed.
            IF ((toks_new[0] EQ NewLabelList[0]  OR toks_new[0] EQ NewLabelList[1]  OR $
                 toks_new[0] EQ NewLabelList[6]  OR toks_new[0] EQ NewLabelList[7]  OR $
                 toks_new[0] EQ NewLabelList[11] OR toks_new[0] EQ NewLabelList[12] OR $
                 toks_new[0] EQ NewLabelList[13] OR toks_new[0] EQ NewLabelList[15] OR $
                 toks_new[0] EQ NewLabelList[18]) AND toks_new[1] NE toks_old[1]) THEN $
               diff_header_count += 1

            ; Test lines that have been added.
            IF (toks_new[0] EQ AddedLabelList[0] OR toks_new[0] EQ AddedLabelList[1] OR $
                toks_new[0] EQ AddedLabelList[2] OR toks_new[0] EQ AddedLabelList[3] OR $
                toks_new[0] EQ AddedLabelList[4] OR toks_new[0] EQ AddedLabelList[5] OR $
                toks_new[0] EQ AddedLabelList[6] OR toks_new[0] EQ AddedLabelList[7] OR $
                toks_new[0] EQ AddedLabelList[8] OR toks_new[0] EQ AddedLabelList[9] OR $
                toks_new[0] EQ AddedLabelList[10] OR toks_new[0] EQ AddedLabelList[11]) THEN BEGIN
               diff_header_count += 1
               next_old_line -= 1
            ENDIF
            
            ; Test lines that have been deleted.
            IF (toks_old[0] EQ OldLabelList[16] OR toks_old[0] EQ OldLabelList[22]) THEN BEGIN
               diff_header_count += 1
               next_new_line -= 1
            ENDIF

         ENDIF ELSE BEGIN
            ; Save the count of changed non-header lines.
            diff_body_count += 1
            
            ; Save certain fields in the data tables of old and new files.
            IF (next_new_line GE table_beg_row) THEN BEGIN
               AccumDataFields, next_new_line-table_beg_row, new_line, FieldData
            ENDIF
         ENDELSE
         
      ENDIF ELSE BEGIN
         ; Save the count of unchanged non-header lines.
         IF (next_new_line GE NUM_NEW_HDR_LINES) THEN same_body_count += 1
         
         ; Save certain fields in the data tables of old and new files.
         IF (next_new_line GE table_beg_row) THEN BEGIN
            AccumDataFields, next_new_line-table_beg_row, new_line, FieldData
         ENDIF
      ENDELSE

      next_old_line += 1
      next_new_line += 1
   ENDWHILE
   
   ;---------------------------------------------------------------------------
   ; Print error if counts don't match.
   ; diff_header_count will frequently be one count short, because the quality
   ; flag will not always change. This applies with reduced frequency to other
   ; parameters also, so this can be a problem.
   ;---------------------------------------------------------------------------
   
   IF (STRTRIM(save_MINX_ver_new,2) EQ STRTRIM(save_MINX_ver_old,2)) THEN $
      diff_header_count += 1
   IF (STRTRIM(save_first_lon_new,2) EQ STRTRIM(save_first_lon_old,2)) THEN $
      diff_header_count += 1
   IF (STRTRIM(save_first_lat_new,2) EQ STRTRIM(save_first_lat_old,2)) THEN $
      diff_header_count += 1
;   IF (ABS(save_pcnt_cover_new - save_pcnt_cover_old) GT 6) THEN $ ; some large diffs!
;      diff_header_count += 1
   IF (save_num_pts_new EQ save_num_pts_old) THEN $
      diff_header_count += 1
   IF (save_median_ht_new EQ save_median_ht_old) THEN $
      diff_header_count += 1
   IF (save_max_ht_new EQ save_max_ht_old) THEN $
      diff_header_count += 1
   IF (STRTRIM(save_stddev_ht_new,2) EQ STRTRIM(save_stddev_ht_old,2)) THEN $
      diff_header_count += 1
   IF (STRTRIM(save_uquality_new,2) NE STRTRIM(save_uquality_old,2)) THEN $
      diff_header_count += 1
   IF (STRTRIM(save_aquality_new,2) EQ STRTRIM(save_aquality_old,2)) THEN $
      diff_header_count += 1

   IF (diff_header_count NE NUM_AFFECTED_ENTRIES) THEN BEGIN
      PRINTF, UnitG, 'File: ' + new_path + ' has ' + $
              STRING(FORMAT='(I3)', diff_header_count) + $
              ' differences in header entries. It should have ' + $
              STRING(FORMAT='(I3)', NUM_AFFECTED_ENTRIES)
      num_verify_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF

   total_line_count = diff_body_count + same_body_count + NUM_NEW_HDR_LINES
   IF (total_line_count NE num_lines_new) THEN BEGIN
      PRINTF, UnitG, 'File: ' + new_path + ' has ' + $
              STRING(FORMAT='(I5)', total_line_count) + $
              ' lines counted by segment. It should have ' + $
              STRING(FORMAT='(I5)', num_lines_new)
      num_verify_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   
   ;---------------------------------------------------------------------------
   ; Print error if heights in new data table heights have a problem.
   ;---------------------------------------------------------------------------
   
   ndxs = WHERE(FieldData.zero_ht EQ -99, num_ndxs)
   IF (num_ndxs GT 0) THEN BEGIN
      PRINTF, UnitG, 'File: ' + new_path + ' has -99 in zero-wind hts.'
      num_verify_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   
   ndxs = WHERE(FieldData.fltr_ht EQ -99, num_ndxs)
   IF (num_ndxs GT 0) THEN BEGIN
      PRINTF, UnitG, 'File: ' + new_path + ' has -99 in filtered hts.'
      num_verify_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF
   
   ndxs = WHERE(FieldData.wind_ht EQ -99, num_ndxs)
   IF (num_ndxs GT 0) THEN BEGIN
      PRINTF, UnitG, 'File: ' + new_path + ' has -99 in wind_corr hts.'
      num_verify_errors += 1
      plume_bad_mask[iplume] = 1
   ENDIF

   max_RFP = MAX(FieldData.fire_RP, max_RFP_ndx)
   fire_terr_ht = (save_zero_or_wind EQ 'wind') ? $
      FieldData.terr_ht[max_RFP_ndx] : FieldData.terr_ht[0]
   
   IF (save_zero_or_wind EQ 'zero') THEN BEGIN
      ndxs = WHERE(FieldData.zero_ht GT -9999, num_ndxs)
      IF (num_ndxs GT 0) THEN BEGIN
         med_zero_ht = MEDIAN(FieldData.zero_ht[ndxs])
         max_zero_ht = MAX(FieldData.zero_ht[ndxs])
      ENDIF

      ndxs = WHERE(FieldData.zero_ht GT -9999 AND $
                   FieldData.fltr_ht GT -9999, num_ndxs)
      IF (num_ndxs GT 0) THEN $
         med_diff_ht = MEDIAN(ABS(FieldData.zero_ht[ndxs] - FieldData.fltr_ht[ndxs]))
      
      IF (save_median_ht_new GT -9998 AND fire_terr_ht NE -9999) THEN BEGIN
         IF (ABS(med_zero_ht - fire_terr_ht - save_median_ht_new) GT $
                 med_wind_ht * 0.9) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad median ht.'
            num_verify_errors += 1
            plume_bad_mask[iplume] = 1
         ENDIF
      ENDIF

      IF (save_max_ht_new GT -9998 AND fire_terr_ht NE -9999) THEN BEGIN
         IF (max_zero_ht - fire_terr_ht LT save_max_ht_new - 200) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad max ht.'
            num_verify_errors += 1
            plume_bad_mask[iplume] = 1
         ENDIF
      ENDIF
      
      IF (save_locvar_ht GT -9998) THEN BEGIN
         IF (ABS(med_diff_ht - save_locvar_ht) GT 300) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad filtered ht.'
            num_verify_errors += 1
            plume_bad_mask[iplume] = 1
         ENDIF
      ENDIF
   ENDIF
   
   IF (save_zero_or_wind EQ 'wind') THEN BEGIN
      ndxs = WHERE(FieldData.wind_ht GT -9999, num_ndxs)
      IF (num_ndxs GT 0) THEN BEGIN
         med_wind_ht = MEDIAN(FieldData.wind_ht[ndxs])
         max_wind_ht = MAX(FieldData.wind_ht[ndxs])
      ENDIF
      
      ndxs = WHERE(FieldData.wind_ht GT -9999 AND $
                   FieldData.fltr_ht GT -9999, num_ndxs)
      IF (num_ndxs GT 0) THEN $
         med_diff_ht = MEDIAN(ABS(FieldData.wind_ht[ndxs] - FieldData.fltr_ht[ndxs]))
      
      IF (save_median_ht_new GT -9998 AND fire_terr_ht NE -9999) THEN BEGIN
         IF (ABS(med_wind_ht - fire_terr_ht - save_median_ht_new) GT $
                 med_wind_ht * 0.9) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad median ht.'
            num_verify_errors += 1
            plume_bad_mask[iplume] = 1
         ENDIF
      ENDIF

      IF (save_max_ht_new GT -9998 AND fire_terr_ht NE -9999) THEN BEGIN
         IF (max_wind_ht - fire_terr_ht LT save_max_ht_new - 200) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad max ht.'
            num_verify_errors += 1
         ENDIF
      ENDIF

      IF (save_locvar_ht GT -9998) THEN BEGIN
         IF (ABS(med_diff_ht - save_locvar_ht) GT 300) THEN BEGIN
            PRINTF, UnitG, 'File: ' + new_path + ' has bad filtered ht.'
            num_verify_errors += 1
            plume_bad_mask[iplume] = 1
         ENDIF
      ENDIF
   ENDIF

ENDFOR

PRINTF, UnitG, (num_verify_errors EQ 0) ? '   No verify errors found.' : $
        '   ' + STRTRIM(STRING(num_verify_errors),2) + $
        ' verify errors were found.'
        
toks0 = 0
toks1 = 0
toks_old = 0
toks_new = 0
ndxs = 0
ndxs1 = 0

;---------------------------------------------------------------------------
; Finally validate the updates by counting and comparing the number of files
; of each type and the length of each filename which should be constant for
; each file type.
; First test .txt files (30 characters) and use the number of files found as
; the standard for the rest of the file types.
;---------------------------------------------------------------------------

PRINTF, UnitG, ''
PRINTF, UnitG, 'Beginning Pass to Validate Names of File Copies'
PRINTF, UnitG, '-----------------------------------------------'

len_orbit_subdir = STRLEN(orbit_subdir) + 1

FindUnmatchedRandB, UnitG, PLUME_new_proj_dir, '0*/Plumes*.txt', $
                    num_found, num_bad_match, bad_match_list
   
IF (num_bad_match GT 0) THEN BEGIN
   PRINTF, UnitG, 'Mismatched red-band and blue-band files:'
   FOR ii=0,num_bad_match-1 DO PRINTF, UnitG, '   ' + bad_match_list[ii]
   PRINTF, UnitG, ' '
   num_filename_errors += 1
ENDIF
bad_match_list = 0

number_expected1 = num_found / 2.0
number_expected2 = FIX(number_expected1)

IF (number_expected2 NE FIX(number_expected1)) THEN BEGIN
   PRINTF, UnitG, 'ERROR - Odd number of .txt files found.'
   num_filename_errors += 1
ENDIF

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/Plumes*.txt', $
                    30+len_orbit_subdir, number_expected2 * 2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...HtWindHist.png files (34 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*HtWindHist.png', $
                    34+len_orbit_subdir, number_expected2 * 2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...HtWindPlot.png files (34 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*HtWindPlot.png', $
                    34+len_orbit_subdir, number_expected2 * 2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...AerosolHist.png files (35 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*AerosolHist.png', $
                    35+len_orbit_subdir, number_expected2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...PlumePoints_An.jpg files (38 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*PlumePoints_An.jpg', $
                    38+len_orbit_subdir, number_expected2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...PlumeContours_An.jpg files (40 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*PlumeContours_An.jpg', $
                    40+len_orbit_subdir, number_expected2 * 2, num_found, $
                    num_bad_length, num_filename_errors
   
;---------------------------------------------------------------------------
; Test ...PlumeAnimation.mp4 files (38 characters).
;---------------------------------------------------------------------------

TestFilenameLength, UnitG, PLUME_new_proj_dir, '0*/*PlumeAnimation.mp4', $
                    38+len_orbit_subdir, number_expected2, num_found, $
                    num_bad_length, num_filename_errors
   
PRINTF, UnitG, (num_filename_errors EQ 0) ? '   No filename errors found.' : $
        '   ' + STRTRIM(STRING(num_filename_errors),2) + $
        ' filename errors were found.'

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

total_errors = num_analyze_errors + num_verify_errors + num_filename_errors

ndx_bad = WHERE(plume_bad_mask, num_bad, COMPLEMENT=ndx_OK, NCOMPLEMENT=num_OK)

IF (total_errors EQ 0) THEN BEGIN
   PRINTF, UnitG, ' '
   PRINTF, UnitG, '------------------'
   PRINTF, UnitG, 'No errors found'
   PRINTF, UnitG, '   # of good plumes = ' + STRTRIM(STRING(num_OK),2)
ENDIF ELSE BEGIN
   PRINTF, UnitG, ' '
   PRINTF, UnitG, '----------------------------------'
   PRINTF, UnitG, 'Total number of errors found = ' + $
           STRTRIM(STRING(total_errors),2)
   PRINTF, UnitG, '   # of good plumes = ' + STRTRIM(STRING(num_OK),2)
   PRINTF, UnitG, '   # of bad plumes  = ' + STRTRIM(STRING(num_bad),2)
ENDELSE

PRINTF, UnitG, ''

FREE_LUN, UnitG
PlumeListAry = 0
plume_bad_mask = 0
file_array_old = 0
file_array_new = 0
FieldData.terr_ht = 0
FieldData.zero_ht = 0
FieldData.wind_ht = 0
FieldData.fltr_ht = 0
FieldData.fire_RP = 0
FieldData = 0
ndx_bad = 0
OldLabelList = 0
NewLabelList = 0

mssg = ['Error log file can be found at:', error_log_filename]
rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)

END  ;  Update_SmokePlumeFiles_Global2008
