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
PRO RetrievePlumeProjects, ProjData
;***************************************************************************
; Set up the data that identifies all the projects in the plume database.
; ProjData structure members:
;
; TopLevelDir is the name of the upper level directory(s) (beneath the parent
;    /data/plume/WebResults/) where data files are found.
; SecLevelDir is the name of the lower level directory (beneath TopLevelDir)
;    where data files are found. It is also the single directory
;    name assigned to the project at the DAAC (under a parent directory for
;    the Plume Project). It also doubles as the PROJECT NAME.
; Code is a consecutive, unique number and is included here to ensure that
;    the array is ordered properly.
; Type defines the aerosol type for project:
;    smoke (=S), volcanic ash (=V), dust (=D).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumProjects = 12 ; ALWAYS UPDATE THIS BEFORE ADDING PROJECTS
ProjInst = { PStruct, TopLevelDir : '', SecLevelDir : '', Code : 0, Type : ''}
ProjData = REPLICATE(ProjInst, NumProjects)
ndx = -1

;***************************************************************************
; Beginning of global 2008 data digitized in 2013/2014.
;                                Top Level Dir   Second Level Dir  Index  Type
ProjData[(ndx=0)]  = { PStruct, 'Global-2008',  'Global-2008_Jan',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Feb',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Mar',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Apr',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_May',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Jun',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Jul',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Aug',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Sep',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Oct',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Nov',  ndx,  'S'}
ProjData[(ndx+=1)] = { PStruct, 'Global-2008',  'Global-2008_Dec',  ndx,  'S'}
   
END  ;  RetrievePlumeProjects

;***************************************************************************
PRO GetHeaderValue, UnitIn, FileName, KeyString, Value, Status
;***************************************************************************
; Extract the value of the parameter from the line buffer.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1
Value = ''

buff = ''
READF, UnitIn, buff

tokens = STRSPLIT(buff, ':', /EXTRACT, COUNT=numtok)
IF (numtok EQ 2) THEN BEGIN
   keystr = STRTRIM(tokens[0],2)
   IF (keystr EQ KeyString) THEN BEGIN
      Value = STRTRIM(tokens[1],2)
   ENDIF ELSE BEGIN
      PRINT, 'Could not extract value for ' + buff
      PRINT, '   from file : ' + FileName
      RETURN
   ENDELSE
ENDIF ELSE BEGIN
   IF (numtok EQ 4) THEN BEGIN
      keystr = STRTRIM(tokens[0],2)
      IF (keystr EQ KeyString) THEN BEGIN
         Value = STRTRIM(tokens[1],2) + ':' + STRTRIM(tokens[2],2) + ':' + $
            STRTRIM(tokens[3],2)
      ENDIF ELSE BEGIN
         PRINT, 'Could not extract value from file for: ' + buff
         RETURN
      ENDELSE
   ENDIF ELSE BEGIN
      PRINT, 'Could not extract value from file for: ' + buff
      RETURN
   ENDELSE
ENDELSE

IF (STRMATCH(Value, '\*\*') OR STRMATCH(Value, '--')) THEN BEGIN
   RETURN
ENDIF

buff = 0
tokens = 0

Status = 0

END  ;  GetHeaderValue

;***************************************************************************
PRO ReadHeaderData, UnitIn, FileName, PlumeNum, GeoRgnNames, DataArray, Status
;***************************************************************************
; Read the header data in the current file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = 0   ; not used!
buff = ''

bad_geo_rgn_class = -9
bad_biome_class   = -99
bad_height        = -9999
bad_power         = -9999

;---------------------------------------------------------------------------
; Read the data line-by-line.
;---------------------------------------------------------------------------

GetHeaderValue, UnitIn, FileName, 'Orbit number', orbit, status1
DataArray[PlumeNum].orbit = orbit
GetHeaderValue, UnitIn, FileName, 'Path number', path, status1
DataArray[PlumeNum].path = path
GetHeaderValue, UnitIn, FileName, 'Block number', block, status1
DataArray[PlumeNum].block = block
GetHeaderValue, UnitIn, FileName, 'Date acquired', date, status1
DataArray[PlumeNum].date_acq = date
GetHeaderValue, UnitIn, FileName, 'UTC time', time, status1
DataArray[PlumeNum].time_acq = time
GetHeaderValue, UnitIn, FileName, 'MINX version', version, status1
DataArray[PlumeNum].minx_version = version
GetHeaderValue, UnitIn, FileName, 'User name', user, status1
DataArray[PlumeNum].digitize_tech = user

FOR iskip=1,2 DO READF, UnitIn, buff

GetHeaderValue, UnitIn, FileName, 'Region name', name, status1
DataArray[PlumeNum].aer_rgn_name = name
GetHeaderValue, UnitIn, FileName, 'Region aerosol type', rgn_type, status1
DataArray[PlumeNum].aer_rgn_type = rgn_type
GetHeaderValue, UnitIn, FileName, 'Region geometry type', geom_type, status1
DataArray[PlumeNum].geom_type = geom_type
GetHeaderValue, UnitIn, FileName, 'Region wind dir type', wind_type, status1
DataArray[PlumeNum].wind_type = wind_type
GetHeaderValue, UnitIn, FileName, 'Retrieved with band', band_type, status1
IF (STRMATCH(band_type, '*Red*'))  THEN band_type = 'Red '
IF (STRMATCH(band_type, '*Blue*')) THEN band_type = 'Blue'
DataArray[PlumeNum].band_type = band_type

FOR iskip=1,9 DO READF, UnitIn, buff

GetHeaderValue, UnitIn, FileName, 'First point longitude', longitude, status1
DataArray[PlumeNum].longitude = longitude
GetHeaderValue, UnitIn, FileName, 'First point latitude', latitude, status1
DataArray[PlumeNum].latitude = latitude
GetHeaderValue, UnitIn, FileName, 'Geographic region', geo_rgn, status1
ndx = WHERE(geo_rgn EQ GeoRgnNames, numndx)
DataArray[PlumeNum].geo_rgn_class = (numndx EQ 1) ? ndx : bad_geo_rgn_class
GetHeaderValue, UnitIn, FileName, 'Biome IGBP name, class', biome, status1
toks = STRSPLIT(biome, ',', /EXTRACT, COUNT=numtoks)
DataArray[PlumeNum].biome_class = (numtoks GT 1) ? FIX(toks[1]) : bad_biome_class
GetHeaderValue, UnitIn, FileName, 'Red/blue band better?', red_blue, status1
DataArray[PlumeNum].red_blue_best = red_blue
GetHeaderValue, UnitIn, FileName, 'Perimeter length (km)', perimeter, status1
DataArray[PlumeNum].perimeter_km = perimeter
GetHeaderValue, UnitIn, FileName, 'Area (sq km)', area, status1
DataArray[PlumeNum].area_km2 = area
GetHeaderValue, UnitIn, FileName, 'Area per point (sq km)', area_per_pt,status1
DataArray[PlumeNum].area_per_pt = area_per_pt

READF, UnitIn, buff

GetHeaderValue, UnitIn, FileName, 'Percent area covered', pcnt_area_ok, status1
DataArray[PlumeNum].hts_pcnt_area_ok = FIX(pcnt_area_ok)
GetHeaderValue, UnitIn, FileName, 'Fire elev. (m > MSL)', fire_ht, status1
DataArray[PlumeNum].elev_at_fire = IsNumber(fire_ht, 1) ? $
   FLOAT(fire_ht) : bad_height
GetHeaderValue, UnitIn, FileName, 'Median ht (m > fire)', med_ht, status1
DataArray[PlumeNum].hts_med_gt_fire = IsNumber(med_ht, 1) ? $
   FLOAT(med_ht) : bad_height
GetHeaderValue, UnitIn, FileName, 'Max ht (m > fire)', max_ht, status1
DataArray[PlumeNum].hts_max_gt_fire = IsNumber(max_ht, 1) ? $
   FLOAT(max_ht) : bad_height
GetHeaderValue, UnitIn, FileName, 'Ht std. deviation (m)', hts_stddev, status1
DataArray[PlumeNum].hts_stddev_new = IsNumber(hts_stddev, 1) ? $
   FLOAT(hts_stddev) : bad_height
GetHeaderValue, UnitIn, FileName, 'Ht local variation (m)', hts_rmsdev, status1
DataArray[PlumeNum].hts_rmsdev_new = IsNumber(hts_rmsdev, 1) ? $
   FLOAT(hts_rmsdev) : bad_height
GetHeaderValue, UnitIn, FileName, '|WndDir-AlongDir|(deg)', diff_along, status1
IF ((diff_along EQ 0 AND DataArray[PlumeNum].hts_max_gt_fire LT -999.) OR $
    diff_along LT 0) THEN diff_along = -9.
DataArray[PlumeNum].diff_dir_along = diff_along
GetHeaderValue, UnitIn, FileName, 'Total fire power (MW)', power, status1
DataArray[PlumeNum].total_power = (power EQ 'NA') ? bad_power : $
   ((ROUND(FLOAT(power)) LE 0) ? bad_power : ROUND(FLOAT(power)))
GetHeaderValue, UnitIn, FileName, 'Retrieval quality est.', auto_quality, status1
DataArray[PlumeNum].auto_quality = auto_quality
GetHeaderValue, UnitIn, FileName, 'Plume has pyro-cumulus', has_pyrocum, status1
DataArray[PlumeNum].has_pyrocum = has_pyrocum
   
END  ;  ReadHeaderData

;***************************************************************************
PRO ReadPointData, UnitIn, NumInFields, NumPts, PlumeNum, PointStruct, $
                   DataArray, Status
;***************************************************************************
; Read the digitized point data in the current file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1
buff = ''

IF (NumPts LE 0) THEN RETURN

;---------------------------------------------------------------------------
; Create the array of point structures for this plume and do initialization.
; Fire power must be the last field!
;---------------------------------------------------------------------------

point_array = REPLICATE(PointStruct, NumPts)
total_num_pts = 0
zwh_num_pts = 0
wch_num_pts = 0
bad_power = -999

;---------------------------------------------------------------------------
; Set up error handling to detect the end of file, and read data line-by-line.
;---------------------------------------------------------------------------

ON_IOERROR, doneread

WHILE (~EOF(UnitIn)) DO BEGIN
   READF, UnitIn, buff
   tokens = STRSPLIT(buff, ' ', /EXTRACT, COUNT=numtok)
   IF (numtok GE NumInFields - 1) THEN BEGIN  ; adapt if missing fire power field
      nexttok = -1
      point_array[total_num_pts].point_num    = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].longitude    = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].latitude     = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].block        = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].sample       = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].line         = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].km_to_pt1    = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].dg_cw_from_N = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].terr_elev    = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].zero_wind_ht = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].corr_wind_ht = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].smoothed_ht  = FIX  (STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].wind_cross   = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].wind_along   = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].wind_total   = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].tau_blue     = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].tau_green    = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].tau_red      = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].tau_nir      = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].ssalb_blue   = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].ssalb_green  = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].ssalb_red    = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].ssalb_nir    = FLOAT(STRTRIM(tokens[(nexttok+=1)],2)) ; not used
      point_array[total_num_pts].tau_frac_sml = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].tau_frac_med = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].tau_frac_lrg = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].tau_frac_sph = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      point_array[total_num_pts].ang_exp      = FLOAT(STRTRIM(tokens[(nexttok+=1)],2))
      
      IF (numtok EQ NumInFields) THEN BEGIN
         power_str = STRTRIM(tokens[(nexttok+=1)],2)
         IF (STRMATCH(power_str,'*NA*') OR STRMID(power_str,0,3) EQ '0.0' OR $
            STRMATCH(power_str,'-99.9')) THEN BEGIN
            point_array[total_num_pts].pnt_tot_power = bad_power
         ENDIF ELSE BEGIN
            point_array[total_num_pts].pnt_tot_power = FLOAT(power_str)
         ENDELSE
      ENDIF ELSE BEGIN
         IF (numtok EQ NumInFields - 1) THEN BEGIN
            point_array[total_num_pts].pnt_tot_power = bad_power
         ENDIF ELSE BEGIN
            mssg = ['Wrong number of fields in line ' + STRTRIM(STRING(numtok),2) + $
                    ' for plume ' + DataArray[PlumeNum].aer_rgn_name, ' ', buff]
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         ENDELSE
      ENDELSE
      total_num_pts += 1
   ENDIF ELSE BEGIN
      mssg = ['Wrong number of fields in line ' + STRTRIM(STRING(numtok),2) + $
              ' for plume ' + DataArray[PlumeNum].aer_rgn_name, ' ', buff]
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      CONTINUE
   ENDELSE
ENDWHILE

;---------------------------------------------------------------------------
; Branch here when done reading points.
;---------------------------------------------------------------------------

doneread:
ON_IOERROR, NULL
IF (total_num_pts NE NumPts) THEN BEGIN
   mssg = ['Number of points read does not match number expected.', $
           '   Expected = ' + STRTRIM(STRING(NumPts),2) + $
           ';  Found = ' + STRTRIM(STRING(total_num_pts),2), $
           '           Continuing to process.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
ENDIF

;---------------------------------------------------------------------------
; Record the number of zero-wind hts and the number of wind-corrected
; heights in the table. The total number of points in table has already been
; validated and recorded
;---------------------------------------------------------------------------

ndxs = WHERE(point_array[*].zero_wind_ht GT -9990, numpix)
DataArray[PlumeNum].zwh_num_pts = numpix

ndxs = WHERE(point_array[*].corr_wind_ht GT 0, numpix)
DataArray[PlumeNum].wch_num_pts = numpix

;---------------------------------------------------------------------------
; Get the number of fire pixels by counting points with valid power (FRP).
;; Also get the total plume power by summing all valid fire powers. Just use
;; it to validate the value read from the header.
;---------------------------------------------------------------------------

ndxs = WHERE(point_array[*].pnt_tot_power GT 0.0, numpix)
IF (numpix GT 0) THEN  DataArray[PlumeNum].num_fire_pix = numpix

;---------------------------------------------------------------------------
; Save the 275 m. sample and line numbers from the head point of the plume
; in the plume structure.
;---------------------------------------------------------------------------

DataArray[PlumeNum].samp_at_headpt = point_array[0].sample
DataArray[PlumeNum].line_at_headpt = point_array[0].line

;---------------------------------------------------------------------------
; Assign the array of point structures to the correct structure in the data
; array.
;---------------------------------------------------------------------------

DataArray[PlumeNum].points = PTR_NEW(point_array)

ndxs = 0
point_array = 0
Status = 0
   
END  ;  ReadPointData

;***************************************************************************
PRO ProcessRegionData, BaseDir, NumOutFields, RgnNum, DataArray, Status
;***************************************************************************
; Process the primary plume data.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1
buff = ''

;---------------------------------------------------------------------------
; Create a structure to contain statistics for plumes.
;---------------------------------------------------------------------------

OrbitNum = 0
RgnNum = 0

;---------------------------------------------------------------------------
; Create a structure to contain data for region files. Each entry corresponds
; to a data field in the output summary file.
;---------------------------------------------------------------------------

data_struct = {              $
   aer_rgn_num :      -9999, $
   aer_rgn_name :        '', $
   path :               -99, $
   samp_at_headpt :    -999, $
   line_at_headpt :     -99, $
   longitude :        -99.9, $
   latitude :         -99.9, $
   date_acq :            '', $
   time_acq :            '', $
   ;
   geo_rgn_class :       -9, $
   biome_class :        -99, $
   red_blue_best :       '', $
   has_pyrocum :         '', $
   auto_quality :        '', $
   ;
   perimeter_km :      -999, $
   area_km2 :         -9999, $
   area_per_pt :     -99.99, $
   elev_at_fire :     -9999, $
   terr_elev_mean :   -9999, $
   terr_elev_stddev :  -999, $
   ;
   num_fire_pix :       -99, $
   total_power :      -9999, $
   total_num_pts :    -9999, $
   zwh_num_pts :      -9999, $  ; zwh = zero-wind height
   wch_num_pts :      -9999, $  ; wch = wind-corrected height
   hts_pcnt_area_ok :   -99, $
   hts_max_gt_fire :  -9999, $
   hts_med_gt_fire :  -9999, $
   hts_stddev_new :    -999, $
   hts_rmsdev_new :    -999, $
   ;
   mean_wind_dir :      -99, $
   diff_dir_along :      -9, $
   mean_total_wind :   -9.9, $
   stddev_total_wnd :  -9.9, $
   mean_cross_wind :  -99.9, $
   mean_along_wind :  -99.9, $
   ;
   mean_tau_green :   -9.99, $
   mean_ssalb_green : -9.99, $
   mean_ang_exp :     -9.99, $
   mean_tau_pcnt_sml :  -99, $
   mean_tau_pcnt_med :  -99, $
   mean_tau_pcnt_lrg :  -99, $
   mean_tau_pcnt_sph :  -99, $
   ;
   minx_version :        '', $
   digitize_tech :       '', $
   ;
   orbit :             -99L, $  ; extract from aer_rgn_name
   block :              -99, $  ; extract from aer_rgn_name
   rgn_in_block :       -99, $  ; extract from aer_rgn_name
   aer_rgn_type :        '', $  ; extract from aer_rgn_name
   geom_type :           '', $  ; extract from aer_rgn_name
   wind_type :           '', $  ; extract from aer_rgn_name
   band_type :           '', $  ; extract from aer_rgn_name
   ;
   points :      PTR_NEW()  $  ; don't count this as a field either
}

DataArray = [data_struct]

;---------------------------------------------------------------------------
; Count the number of entries in the structure. This should match the number
; of data fields in the output summary file, except subtract 8: 7 for the
; data fields not explicitly written to file (they are components of the
; aerosol region name) and 1 for the points pointer.
;---------------------------------------------------------------------------

NumOutFields = N_TAGS(data_struct) - 8

;---------------------------------------------------------------------------
; Create a structure to contain digitized point data for region files.
;---------------------------------------------------------------------------

point_struct = {        $
   point_num :       0, $
   longitude :     0.0, $
   latitude :      0.0, $
   block :           0, $
   sample :          0, $
   line :            0, $
   km_to_pt1 :     0.0, $
   dg_cw_from_N :    0, $
   terr_elev :       0, $
   zero_wind_ht :    0, $
   corr_wind_ht :    0, $
   smoothed_ht :     0, $
   wind_cross :    0.0, $
   wind_along :    0.0, $
   wind_total :    0.0, $
   tau_blue :      0.0, $
   tau_green :     0.0, $
   tau_red :       0.0, $
   tau_nir :       0.0, $
   ssalb_blue :    0.0, $
   ssalb_green :   0.0, $
   ssalb_red :     0.0, $
   ssalb_nir :     0.0, $
   ang_exp :       0.0, $
   tau_frac_sml :  0.0, $
   tau_frac_med :  0.0, $
   tau_frac_lrg :  0.0, $
   tau_frac_sph :  0.0, $
   pnt_tot_power : 0.0  $
}

;---------------------------------------------------------------------------
; Count the number of entries in the structure. This should match the number
; of data fields in the raw plume text files.
;---------------------------------------------------------------------------

NumInFields = N_TAGS(point_struct)

;---------------------------------------------------------------------------
; Collect the subdirectory names of all orbit/block-range groups.
; NOTE - 0????? will be invalid when orbit numbering passes 99999
;---------------------------------------------------------------------------

dir_list = FILE_SEARCH(BaseDir + '[0-9]?????', /TEST_DIRECTORY, COUNT=OrbitNum)

;---------------------------------------------------------------------------
; Get the parameters needed to create the different geographic region maps.
;---------------------------------------------------------------------------

GetLocationBaseParams, NumGeoRgns, TextBoxHt, MapParams

;---------------------------------------------------------------------------
; Define the IGBP geographic region names.
;---------------------------------------------------------------------------

GetGeoRegionParams, NumGeoRgns, PolyPtr, GeoNames, GeoColors, GeoRgnParams

NdxStr = ', ' + STRTRIM(STRING(INDGEN(NumGeoRgns)),2)
GeoRgnNames = GeoNames + NdxStr

FOR irgn=0,NumGeoRgns-1 DO PTR_FREE, PolyPtr[irgn]
PolyPtr = 0
GeoNames = 0
GeoColors = 0
GeoRgnParams = 0

;---------------------------------------------------------------------------
; Loop over all the orbit/blockrange subdirectories.
;---------------------------------------------------------------------------

FOR idir=0,OrbitNum-1 DO BEGIN

   ;-----------------------------------------------------------------------
   ; Collect the file names of plumes and clouds in the current directory.
   ;------------------------------------------------------------------------
   
   file_template = dir_list[idir] + !KON.Misc.Slash + '*.txt'
   filelist = FILE_SEARCH(file_template, /TEST_REGULAR, COUNT=numfile)
   
   ;------------------------------------------------------------------------
   ; Loop over all the orbit/block-range subdirectories.
   ;------------------------------------------------------------------------
   
   FOR ifile=0,numfile-1 DO BEGIN
      
      ;---------------------------------------------------------------------
      ; Open the file for reading and read the header data.
      ;---------------------------------------------------------------------
      
      OPENR, unit_in, filelist[ifile], /GET_LUN
      
      ReadHeaderData, unit_in, filelist[ifile], RgnNum, GeoRgnNames, $
                      DataArray, status1
      
      ;---------------------------------------------------------------------
      ; Skip down to the digitized point data.  The starting point is in the
      ; section beginning with "RESULTS:" and after two successive lines
      ; which begin with 'Pt#' and '---' respectively. Also on the "RESULTS:"
      ; line get the number of total retrieved points in the file.
      ;---------------------------------------------------------------------
      
      tok1 = ''
      tok2 = ''
      WHILE (tok1 NE 'RESULTS:') DO BEGIN
         READF, unit_in, buff
         tokens = STRSPLIT(buff, ' ', /EXTRACT, COUNT=numtok)
         IF (numtok GT 0) THEN tok1 = tokens[0]
         IF (numtok GT 1) THEN tok2 = tokens[1]
      ENDWHILE
      DataArray[RgnNum].total_num_pts = FIX(tok2)
      
      tok1 = ''
      WHILE (tok1 NE 'Pt#') DO BEGIN
         READF, unit_in, buff
         tokens = STRSPLIT(buff, ' ', /EXTRACT, COUNT=numtok)
         IF (numtok GT 0) THEN tok1 = tokens[0]
      ENDWHILE
      
      READF, unit_in, buff
      tokens = STRSPLIT(buff, ' ', /EXTRACT, COUNT=numtok)
      IF (numtok GT 0) THEN BEGIN
         tok1 = tokens[0]
         IF (tok1 NE '---' AND tok1 NE '----' AND tok1 NE '------') THEN BEGIN
            PRINT, 'Could not find point header 2 for file: '
            PRINT, '   ', filelist[ifile]
            CONTINUE
         ENDIF
      ENDIF ELSE BEGIN
         PRINT, 'Could not find point header 1 for file: '
         PRINT, '   ', filelist[ifile]
         CONTINUE
      ENDELSE
      
      ;---------------------------------------------------------------------
      ; Save the file position for the beginning of the point data. Count
      ; the number of records (points) that follow. Then position the file
      ; back to the beginning of the point data.
      ;---------------------------------------------------------------------
      
      POINT_LUN, (-unit_in), position
      
      num_points = 0
      WHILE (1) DO BEGIN
         IF (EOF(unit_in)) THEN BREAK
         READF, unit_in, buff
         num_points += 1
      ENDWHILE
      
      POINT_LUN, unit_in, position
      
      IF (num_points NE DataArray[RgnNum].total_num_pts) THEN BEGIN
         msg = ['Number of points does not match for region: ', $
            DataArray[RgnNum].aer_rgn_name]
         rval = DIALOG_MESSAGE(msg, /ERROR, /CENTER)
         CONTINUE
      ENDIF
      
      ;---------------------------------------------------------------------
      ; Read the digitized point data in the current file.
      ;---------------------------------------------------------------------
      
      ReadPointData, unit_in, NumInFields, num_points, RgnNum, point_struct, $
                     DataArray, status2
         
      FREE_LUN, unit_in
      
      ;---------------------------------------------------------------------
      ; Increment the number of plumes read, and concatenate another empty
      ; instance of the data structure to the data array.
      ;---------------------------------------------------------------------
      
      RgnNum += 1
      
      DataArray = [DataArray, data_struct]
      
      tokens = 0
      buff = ''
   ENDFOR
   
   filelist = 0

ENDFOR

dir_list = 0
Status = 0
   
END  ;  ProcessRegionData

;***************************************************************************
PRO GenerateRegionStats, NumRegions, DataArray, Status
;***************************************************************************
; Generate statistics for the data and write to file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Loop over the plumes and get statistics on the data in the points.
;---------------------------------------------------------------------------

FOR iplume=0,NumRegions-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Skip this if there are no points.
   ;------------------------------------------------------------------------
   
   IF (~ PTR_VALID(DataArray[iplume].points)) THEN BEGIN
      mssg = ['Bad pointer to points in plume: ', $
              DataArray[iplume].aer_rgn_name, '     Continuing to process.']
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      CONTINUE
   ENDIF
   
   ;------------------------------------------------------------------------
   ; Compute the statistics.
   ;------------------------------------------------------------------------
   
   val = (*(DataArray[iplume].points))[*].dg_cw_from_N
   good_ndx = WHERE(val GT -99., numgood)
   IF (numgood GT 0) THEN BEGIN
      val1 = val[good_ndx]
      ndxs1 = WHERE(val1 GE   0. AND val1 LT  30., numndxs1)
      ndxs2 = WHERE(val1 LE 360. AND val1 GT 330., numndxs2)
      IF (numndxs1 GT 0 AND numndxs2 GT 0) THEN BEGIN
         val1[ndxs1] += 360.
         mean_dir = MEAN(val1)
         IF (mean_dir GT 360.) THEN mean_dir -= 360.
         DataArray[iplume].mean_wind_dir = mean_dir
      ENDIF ELSE BEGIN
         DataArray[iplume].mean_wind_dir = MEAN(val[good_ndx])
      ENDELSE
      val1 = 0
      ndxs1 = 0
      ndxs2 = 0
   ENDIF
   
   val = (*(DataArray[iplume].points))[*].terr_elev
   good_ndx = WHERE(val GT -999, numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].terr_elev_mean = MEAN(val[good_ndx])
   IF (numgood GT 1) THEN $
      DataArray[iplume].terr_elev_stddev = FIX(STDDEV(val[good_ndx]))
      
   val = (*(DataArray[iplume].points))[*].wind_total
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_total_wind = MEAN(val[good_ndx])
   IF (numgood GT 1) THEN $
      DataArray[iplume].stddev_total_wnd = STDDEV(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].wind_cross
   good_ndx = WHERE(val GT -99., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_cross_wind = MEAN(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].wind_along
   good_ndx = WHERE(val GT -99., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_along_wind = MEAN(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].tau_green
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_tau_green = MEAN(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].ssalb_green
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_ssalb_green = MEAN(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].tau_frac_sml
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN DataArray[iplume].mean_tau_pcnt_sml = $
      ROUND(MEAN(val[good_ndx]) * 100.0)
      
   val = (*(DataArray[iplume].points))[*].tau_frac_med
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN DataArray[iplume].mean_tau_pcnt_med = $
      ROUND(MEAN(val[good_ndx]) * 100.0)
      
   val = (*(DataArray[iplume].points))[*].tau_frac_lrg
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN DataArray[iplume].mean_tau_pcnt_lrg = $
      ROUND(MEAN(val[good_ndx]) * 100.0)
      
   val = (*(DataArray[iplume].points))[*].tau_frac_sph
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN DataArray[iplume].mean_tau_pcnt_sph = $
      ROUND(MEAN(val[good_ndx]) * 100.0)
      
   val = (*(DataArray[iplume].points))[*].ang_exp
   good_ndx = WHERE(val GT -9., numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].mean_ang_exp = MEAN(val[good_ndx])
      
   val = (*(DataArray[iplume].points))[*].pnt_tot_power
   good_ndx = WHERE(val GT 0.0, numgood)
   IF (numgood GT 0) THEN $
      DataArray[iplume].total_power = ROUND(TOTAL(val[good_ndx]))
ENDFOR

good_ndx = 0

END  ;  GenerateRegionStats

;***************************************************************************
PRO WriteOneLineToFile, UnitOut, RgnNmbr, RgnElement
;***************************************************************************
; Write one line of summary file data to a file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Write line to file.
;---------------------------------------------------------------------------

FormatString = '(I5,1X,A19,1X,I3,1X,I4,1X,I3,1X,F8.3,1X,F7.3,1X,A10,1X,'   + $
               'A8,1X,I2,1X,I3,1X,3(A2,1X),I4,1X,I5,1X,F6.3,1X,2(I5,1X),' + $
               'I4,1X,I3,1X,4(I6,1X),I4,1X,2(I5,1X),2(I5,1X),I3,1X,I2,'   + $
               '1X,2(F4.1,1X),2(F5.1,1X),3(F5.2,1X),4(I3,1X),A4,1X,A6)'
   
PRINTF, UnitOut, FORMAT=FormatString, $
        RgnNmbr, $                                      ;  1
        RgnElement.aer_rgn_name, $                      ;
        RgnElement.path, $                              ;
        RgnElement.samp_at_headpt, $                    ;
        RgnElement.line_at_headpt, $                    ;
        RgnElement.longitude, $                         ;
        RgnElement.latitude, $                          ;
        RgnElement.date_acq, $                          ;
        RgnElement.time_acq, $                          ;
;
        RgnElement.geo_rgn_class, $                     ; 10
        RgnElement.biome_class, $                       ;
        STRMID(RgnElement.red_blue_best,0,1), $         ;
        STRMID(RgnElement.has_pyrocum,  0,1), $         ;
        STRMID(RgnElement.auto_quality, 0,1), $         ;
;
        RgnElement.perimeter_km, $                      ;
        RgnElement.area_km2, $                          ;
        RgnElement.area_per_pt, $                       ;
        RgnElement.elev_at_fire, $                      ;
        RgnElement.terr_elev_mean, $                    ;
        RgnElement.terr_elev_stddev, $                  ; 20
;
        RgnElement.num_fire_pix, $                      ;
        RgnElement.total_power, $                       ;
        RgnElement.total_num_pts, $                     ;
        RgnElement.zwh_num_pts, $                       ;
        RgnElement.wch_num_pts, $                       ;
        RgnElement.hts_pcnt_area_ok, $                  ;
        RgnElement.hts_max_gt_fire, $                   ;
        RgnElement.hts_med_gt_fire, $                   ;
        RgnElement.hts_rmsdev_new, $                    ;
        RgnElement.hts_stddev_new, $                    ; 30
;
        RgnElement.mean_wind_dir, $                     ;
        RgnElement.diff_dir_along, $                    ;
        RgnElement.mean_total_wind, $                   ;
        RgnElement.stddev_total_wnd, $                  ;
        RgnElement.mean_cross_wind, $                   ;
        RgnElement.mean_along_wind, $                   ;
;
        RgnElement.mean_tau_green, $                    ;
        RgnElement.mean_ssalb_green,$                   ;
        RgnElement.mean_ang_exp, $                      ;
        RgnElement.mean_tau_pcnt_sml, $                 ; 40
        RgnElement.mean_tau_pcnt_med, $                 ;
        RgnElement.mean_tau_pcnt_lrg, $                 ;
        RgnElement.mean_tau_pcnt_sph, $                 ;
;
        RgnElement.minx_version, $                      ;
        RgnElement.digitize_tech                        ; 45
  
END  ;  WriteOneLineToFile

;***************************************************************************
PRO WriteRegionDatabase, ProjData, OutBaseDir, OutRegionFile, NumOutFields, $
                         NumRegions, DataArray, Status
;***************************************************************************
; Write the plume results to the project database (a wide ASCII file).
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set the number of fields being printed to the file and prepare the title.
;---------------------------------------------------------------------------

arname = 'smoke'
IF (ProjData.Type EQ 'V') THEN arname = 'volcanic'
IF (ProjData.Type EQ 'D') THEN arname = 'dust'

;---------------------------------------------------------------------------
; Define the format for the summary table. fmtstr1 is the format for the
; column numbers, and fmtstr2 is the format for the data rows.
;---------------------------------------------------------------------------

fmtstr1 = '(I3,1X, I13,1X,I10,1X,I4,1X, I3,1X, I6,1X, I7,1X, I9,1X, ' + $
          'I9,1X, I5,1X, I3,1X, I2,1X, I2,1X, I2,1X, I3,1X, ' + $
          'I5,1X, I6,1X, I5,1X, I5,1X, I4,1X, I4,1X, I5,1X, ' + $
          'I6,1X, I6,1X, I6,1X, I4,1X, I5,1X, I5,1X, I5,1X, I5,1X, ' + $
          'I4,1X, I2,1X, I3,1X, I4,1X, I5,1X, I5,1X, ' + $
          'I5,1X, I5,1X, I5,1X, I4,1X, I3,1X, I3,1X, I3,1X, I3,1X, ' + $
          'I5)'
   
under_scores = '----- ------------------- --- ---- --- -------- ----' + $
               '--- ---------- -------- -- --- -- -- -- ---- ----' + $
               '- ------ ----- ----- ---- --- ------ --' + $
               '---- ------ ------ ---- ----- ----- ----- ----- --' + $
               '- -- ---- ---- ----- ----- ----- ----- ----- --- ---' + $
               ' --- --- ---- ------'
   
;---------------------------------------------------------------------------
; This is a poor man's solution to a sorting problem: There are red-band
; retrievals and blue-band retrievals for every plume. The retrievals are not
; sorted so red and blue lines are together. To sort them, we can write all
; the red-band retrievals to one file and the blue-band retrievals to another.
; Then they can be read in in alternating fashion and written to the final
; summary file. This could be done in memory instead.
;---------------------------------------------------------------------------

file_blu = OutBaseDir + 'Temp_Blu.txt'
file_red = OutBaseDir + 'Temp_Red.txt'

OPENW, unit_blu, file_blu, /GET_LUN
OPENW, unit_red, file_red, /GET_LUN
num_blu = 1
num_red = 1
num_skip = 0

FOR iplume=0,NumRegions-1 DO BEGIN
   blu_red = STRMID(DataArray[iplume].aer_rgn_name, 16, 1)
   IF (STRUPCASE(blu_red) EQ 'B') THEN BEGIN
      WriteOneLineToFile, unit_blu, num_blu, DataArray[iplume]
      num_blu += 1
   ENDIF ELSE BEGIN
      WriteOneLineToFile, unit_red, num_red, DataArray[iplume]
      num_red += 1
   ENDELSE
ENDFOR

FREE_LUN, unit_blu
FREE_LUN, unit_red

;---------------------------------------------------------------------------
; Open the final summary file for writing. Then write the descriptive header
; and the column definition lines for the summary table.
;---------------------------------------------------------------------------

OPENW, unit_out, OutRegionFile, /GET_LUN

PRINTF, unit_out, 'Table of ' + arname + ' plume/cloud retrieval '    + $
        'results for MISR Plume Height project: "' + ProjData.SecLevelDir  + $
        '".  Refer to the "Project Summary File Description" document ' + $
        'for instructions on its use.'
PRINTF, unit_out, ''
PRINTF, unit_out, FORMAT=fmtstr1, INDGEN(NumOutFields) + 1
PRINTF, unit_out, FORMAT='(A)', under_scores

;---------------------------------------------------------------------------
; Write the summary table. Do a validation step by confirming that the blue
; and red-band retrievals for each plume are always contiguous.
;---------------------------------------------------------------------------

OPENR, unit_blu, file_blu, /GET_LUN
OPENR, unit_red, file_red, /GET_LUN

buff_blu = ''
buff_red = ''

FOR iplume=0,(NumRegions-1-num_skip)/2 DO BEGIN
   READF,  unit_blu, buff_blu
   READF,  unit_red, buff_red
   
   blu_name = STRMID(buff_blu, 6, 16)
   blu_num  = STRMID(buff_blu, 23, 2)
   red_name = STRMID(buff_red, 6, 16)
   red_num  = STRMID(buff_red, 23, 2)
   
   IF (blu_name NE red_name OR blu_num NE red_num) THEN BEGIN
      mssg = ['The blue and red regions names are not contiguous for:', $
              STRMID(buff_blu, 6, 25) + ' and ' + STRMID(buff_red, 6, 25)]
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   ENDIF
   
   PRINTF, unit_out, buff_blu
   PRINTF, unit_out, buff_red
ENDFOR

;---------------------------------------------------------------------------
; DO final cleanup.
;---------------------------------------------------------------------------

FREE_LUN, unit_blu
FREE_LUN, unit_red

FILE_DELETE, file_blu
FILE_DELETE, file_red

FREE_LUN, unit_out
rtrn_val = ChmodCatchError(OutRegionFile, '666'O)
   
fmtstr1 = 0
under_scores = 0

END  ;  WriteRegionDatabase

;***************************************************************************
PRO CollectMapData, NumPlumes, DataArray, PlumeArray, NumGeoRgns, MapParams, $
                    TextBoxHt
;***************************************************************************
; Collect the lat/lon locations of all first points of plumes or clouds in
; preparation for plotting.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Create an array to contain information about each plume (or clouds) in the
; project.
;---------------------------------------------------------------------------

plume_struct = { $
   orbit:  0L, $
   path:    0, $
   block:   0, $
   date:   '', $
   georgn: '', $
   name:   '', $
   type:   '', $
   lon:   0.0, $
   lat:   0.0  $
}

PlumeArray = REPLICATE(plume_struct, NumPlumes)
plume_struct = 0

;---------------------------------------------------------------------------
; Loop over the plumes (or clouds) and extract their relevant information.
;---------------------------------------------------------------------------

FOR irgn=0,NumPlumes-1 DO BEGIN
   PlumeArray[irgn].orbit  = DataArray[irgn].orbit
   PlumeArray[irgn].path   = DataArray[irgn].path
   PlumeArray[irgn].block  = DataArray[irgn].block
   PlumeArray[irgn].date   = DataArray[irgn].date_acq
   PlumeArray[irgn].georgn = DataArray[irgn].geo_rgn_class
   PlumeArray[irgn].name   = DataArray[irgn].aer_rgn_name
   PlumeArray[irgn].type   = 'plume'
   PlumeArray[irgn].lon    = DataArray[irgn].longitude
   PlumeArray[irgn].lat    = DataArray[irgn].latitude
   
   toks = STRSPLIT(DataArray[irgn].aer_rgn_name, '-', COUNT=numtoks, $
                   /EXTRACT)
   IF (numtoks EQ 3) THEN $
      IF (STRLEN(toks[2]) GE 5 AND STRMID(toks[2], 2, 1) EQ 'N') THEN $
         PlumeArray[irgn].type = 'cloud'
ENDFOR
   
toks = 0
   
;---------------------------------------------------------------------------
; Get the parameters needed to create the different geographic region maps.
;---------------------------------------------------------------------------

GetLocationBaseParams, NumGeoRgns, TextBoxHt, MapParams

END  ;  CollectMapData

;***************************************************************************
PRO GetLocationBaseParams, NumGeoRgns, TextBoxHt, MapParams
;***************************************************************************
; Create the index maps that will show the location of each plumes or cloud.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumGeoRgns = 7
TextBoxHt = 20
 
;---------------------------------------------------------------------------
; Create the map structure.
;---------------------------------------------------------------------------

map_params = {           $
   lon_center :     0.0, $
   lon_delta  :     0.0, $
   lon_label  :     0.0, $
   lat_center :     0.0, $
   lat_delta  :     0.0, $
   lat_label  :     0.0, $
   coast_res  :       0, $
   map_scale  :     0.0, $
   map_proj   :       0, $
   map_xsize  :       0, $
   map_ysize  :       0, $
   xmargin    : [0.,0.], $
   ymargin    : [0.,0.]  $
}

MapParams = REPLICATE(map_params, NumGeoRgns)
map_params = 0

;---------------------------------------------------------------------------
; Select the lat/long bounds appropriate to each geographic region.
;---------------------------------------------------------------------------

; Africa - 
   geo_rgn = 0
   MapParams[geo_rgn].lon_center =  18.
   MapParams[geo_rgn].lon_delta  =  20.
   MapParams[geo_rgn].lon_label  = -22.
   MapParams[geo_rgn].lat_center =   2.
   MapParams[geo_rgn].lat_delta  =  20.
   MapParams[geo_rgn].lat_label  = -35.
   MapParams[geo_rgn].coast_res  =   1
   MapParams[geo_rgn].map_scale  = 1.40E8
   MapParams[geo_rgn].map_proj   =   1

; Australia - 
   geo_rgn = 1
   MapParams[geo_rgn].lon_center = 144.
   MapParams[geo_rgn].lon_delta  =  20.
   MapParams[geo_rgn].lon_label  = 167.
   MapParams[geo_rgn].lat_center = -30.
   MapParams[geo_rgn].lat_delta  =  10.
   MapParams[geo_rgn].lat_label  = -50.
   MapParams[geo_rgn].coast_res  =   1
   MapParams[geo_rgn].map_scale  = 0.87E8
   MapParams[geo_rgn].map_proj   =   2

; SouthwestEurasia - 
   geo_rgn = 2
   MapParams[geo_rgn].lon_center =  35.
   MapParams[geo_rgn].lon_delta  =  20.
   MapParams[geo_rgn].lon_label  =   0.
   MapParams[geo_rgn].lat_center =  48.
   MapParams[geo_rgn].lat_delta  =  10.
   MapParams[geo_rgn].lat_label  =  20.
   MapParams[geo_rgn].coast_res  =   1
   MapParams[geo_rgn].map_scale  = 1.05E8
   MapParams[geo_rgn].map_proj   =   2

; NorthAmerica - 
   geo_rgn = 3
   MapParams[geo_rgn].lon_center = -102.
   MapParams[geo_rgn].lon_delta  =   20.
   MapParams[geo_rgn].lon_label  = -140.
   MapParams[geo_rgn].lat_center =  45.
   MapParams[geo_rgn].lat_delta  =   20.
   MapParams[geo_rgn].lat_label  =   13.
   MapParams[geo_rgn].coast_res  =    1
   MapParams[geo_rgn].map_scale  = 1.17E8
   MapParams[geo_rgn].map_proj   =    2

; BorealEurasia - 
   geo_rgn = 4
   MapParams[geo_rgn].lon_center =  97.
   MapParams[geo_rgn].lon_delta  =  20.
   MapParams[geo_rgn].lon_label  = 133.
   MapParams[geo_rgn].lat_center =  65.
   MapParams[geo_rgn].lat_delta  =  10.
   MapParams[geo_rgn].lat_label  =  34.
   MapParams[geo_rgn].coast_res  =   1
   MapParams[geo_rgn].map_scale  = 1.10E8
   MapParams[geo_rgn].map_proj   =   2

; SouthAmerica - 
   geo_rgn = 5
   MapParams[geo_rgn].lon_center =  -60.
   MapParams[geo_rgn].lon_delta  =   20.
   MapParams[geo_rgn].lon_label  =  -32.
   MapParams[geo_rgn].lat_center =  -22.
   MapParams[geo_rgn].lat_delta  =   20.
   MapParams[geo_rgn].lat_label  =  -50.
   MapParams[geo_rgn].coast_res  =    1
   MapParams[geo_rgn].map_scale  = 1.25E8
   MapParams[geo_rgn].map_proj   =    1

; SouthAsia - 
   geo_rgn = 6
   MapParams[geo_rgn].lon_center = 102.
   MapParams[geo_rgn].lon_delta  =  20.
   MapParams[geo_rgn].lon_label  = 150.
   MapParams[geo_rgn].lat_center =  33.
   MapParams[geo_rgn].lat_delta  =  20.
   MapParams[geo_rgn].lat_label  = -17.
   MapParams[geo_rgn].coast_res  =   1
   MapParams[geo_rgn].map_scale  = 1.75E8
   MapParams[geo_rgn].map_proj   =   2

MapParams[*].map_xsize = 300
MapParams[*].map_ysize = 300 + TextBoxHt

MapParams[*].xmargin = [0,0]
MapParams[*].ymargin = [0,1.97]

END  ;  GetLocationBaseParams

;***************************************************************************
PRO MakePlumeLocationMaps, ProjData, OutBaseDir, NumPlumes, PlumeArray, $
                           NumGeoRgns, MapParams, TextBoxHt, FirstOrbit, $
                           TempFile
;***************************************************************************
; Using the appropriate geographic regions's location base map, overpost the
; MISR block rectangle containing the plume as well as the plume location.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

old_font = GetFontInfo(0)

;---------------------------------------------------------------------------
; Get the coordinate outlines of the geographic regions.
;---------------------------------------------------------------------------

GetGeoRegionParams, NumGeoRgns, PolyPtr, GeoNames, GeoColors, GeoRgnParams

;---------------------------------------------------------------------------
; Set an error catcher.
;---------------------------------------------------------------------------

plume_name = ''
CATCH, error_status
IF (error_status NE 0) THEN BEGIN
   mssg = ['Procedure MakePlumeLocationMaps failed on', plume_name]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Create the window to display maps in.
;---------------------------------------------------------------------------

save_window = !D.WINDOW

WINDOW, XSIZE=MapParams[0].map_xsize, YSIZE=MapParams[0].map_ysize, $
        TITLE='', RETAIN=2, /FREE, XPOS=1000, YPOS=600
   
MapWindow = !D.WINDOW
OldGeoRgn = -1

;---------------------------------------------------------------------------
; Process the plumes creating maps showing the location of each. Save the
; previous geographic region so MAP_SET doesn't have to be invoked so often.
; That eventually hangs or crashes the process.
;---------------------------------------------------------------------------

FOR iplume=0,NumPlumes-1 DO BEGIN

   IF (PlumeArray[iplume].orbit LT FirstOrbit) THEN CONTINUE

   plume_name = PlumeArray[iplume].name

   band_used = STRMID(plume_name,16,1)
   IF (band_used EQ 'B') THEN CONTINUE ; only use red bands - need only 1 copy

   ;------------------------------------------------------------------------
   ; Draw the geographic region base maps and return them for reuse.
   ;------------------------------------------------------------------------

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'medium'}

   DrawBaseMap, NumGeoRgns, MapParams, PlumeArray[iplume].georgn, TextBoxHt, $
                PlumeArray[iplume].name, MapWindow, OldGeoRgn

   ;------------------------------------------------------------------------
   ; Draw the boundary of the geographic region containing the plume.
   ;------------------------------------------------------------------------
   
   DrawGeoRgn, NumGeoRgns, PolyPtr, GeoColors, PlumeArray[iplume], MapWindow

   ;------------------------------------------------------------------------
   ; Draw the MISR block containing the plume.
   ;------------------------------------------------------------------------

   DrawBlocks, MapParams, PlumeArray[iplume], MapWindow

   ;------------------------------------------------------------------------
   ; Draw a point at the location of the plume.
   ;------------------------------------------------------------------------
   
   DrawPoints, PlumeArray[iplume], MapWindow
   
   ;------------------------------------------------------------------------
   ; Write a title on the map.
   ;------------------------------------------------------------------------
   
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}

   x_mid = MapParams[PlumeArray[iplume].georgn].map_xsize / 2
   y_mid = MapParams[PlumeArray[iplume].georgn].map_ysize - TextBoxHt + 4
   toks = STRSPLIT(PlumeArray[iplume].date, '-', COUNT=numtoks, /EXTRACT)
   month = (['January','February','March','April','May','June','July', $
             'August','September','October','November','December'])[toks[1]-1]
   day = STRTRIM(STRING(FIX(toks[2])),2)
   plume_date = month + ' ' + day + ', ' + toks[0]
   title = PlumeArray[iplume].name + ' ; ' + plume_date
   
   XYOUTS, x_mid, y_mid, title, ALIGNMENT=0.5, COLOR=0, /DEVICE
   
   SetFontInfo, old_font
        
   ;------------------------------------------------------------------------
   ; Copy the final map image to the subdirectory where the plume resides.
   ;------------------------------------------------------------------------

   CopyAndSave, OutBaseDir, PlumeArray[iplume], MapWindow

   ;------------------------------------------------------------------------
   ; Write to file the name of the last successfully processed plume so if
   ; the program crashes or goes into an endless loop, the user can pick up
   ; where he left off.
   ;------------------------------------------------------------------------

   OPENW, unit_temp, TempFile, /GET_LUN
   PRINTF, unit_temp, 'Name of last plume processed: ' + PlumeArray[iplume].name
   FREE_LUN, unit_temp
ENDFOR

SafeWDELETE, MapWindow, didit

CATCH, /CANCEL

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

SafeWSET, save_window, DidIt

FOR irgn=0,NumGeoRgns-1 DO PTR_FREE, PolyPtr[irgn]

PolyPtr = 0
GeoNames = 0
GeoColors = 0
GeoRgnParams = 0

END  ;  MakePlumeLocationMaps

;***************************************************************************
PRO DrawBaseMap, NumGeoRgns, MapParams, iGeoRgn, TextBoxHt, PlumeName, $
                 MapWindow, OldGeoRgn
;***************************************************************************
; Create base maps for each geographic region and save as pixmap windows.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set map projection, scale etc.
;---------------------------------------------------------------------------

map_scale = MapParams[iGeoRgn].map_scale * 250.0 / MapParams[iGeoRgn].map_xsize

IF (iGeoRgn NE OldGeoRgn) THEN BEGIN
   IF (MapParams[iGeoRgn].map_proj EQ 1) THEN $
      MAP_SET, MapParams[iGeoRgn].lat_center, MapParams[iGeoRgn].lon_center, $
               SCALE=map_scale, /LAMBERT, XMARGIN=MapParams[iGeoRgn].xmargin, $
               YMARGIN=MapParams[iGeoRgn].ymargin
   IF (MapParams[iGeoRgn].map_proj EQ 2) THEN $
      MAP_SET, MapParams[iGeoRgn].lat_center, MapParams[iGeoRgn].lon_center, $
               SCALE=map_scale, /ROBINSON, XMARGIN=MapParams[iGeoRgn].xmargin, $
               YMARGIN=MapParams[iGeoRgn].ymargin
ENDIF

OldGeoRgn = iGeoRgn
ERASE, COLOR=!KON.Colors.lt_blue

;---------------------------------------------------------------------------
; Draw continent fill, lat/long grid lines, coast and country lines.
;---------------------------------------------------------------------------

MAP_CONTINENTS, /CONTINENTS, FILL_CONTINENTS=1, COLOR=!KON.Colors.lt_green
MAP_GRID, LATDEL=MapParams[iGeoRgn].lat_delta, LATS=-90.0,  LATALIGN=0.5, $
          LONDEL=MapParams[iGeoRgn].lon_delta, LONS=-180.0, LONALIGN=0.5, $
          LATLAB=MapParams[iGeoRgn].lon_label, ORIENTATION=0, CHARSIZE=1, $
          LONLAB=MapParams[iGeoRgn].lat_label, COLOR=!KON.Colors.black, $
          LABEL=1
IF (iGeoRgn EQ 3) THEN $  ; Only do for North America
    MAP_CONTINENTS, /USA,   COLOR=!KON.Colors.gray2, MLINETHICK=1
MAP_CONTINENTS, /COUNTRIES, COLOR=!KON.Colors.gray2, MLINETHICK=1
MAP_CONTINENTS, /COASTS,    COLOR=!KON.Colors.gray3, MLINETHICK=1

;---------------------------------------------------------------------------
; Draw fill and lines around the outside edges and around the title box.
;---------------------------------------------------------------------------

edge = TRANSPOSE( $
   [[0, 0, MapParams[iGeoRgn].map_xsize-1, MapParams[iGeoRgn].map_xsize-1, 0], $
    [MapParams[iGeoRgn].map_ysize-TextBoxHt, MapParams[iGeoRgn].map_ysize-2, $
     MapParams[iGeoRgn].map_ysize-2, MapParams[iGeoRgn].map_ysize-TextBoxHt, $
     MapParams[iGeoRgn].map_ysize-TextBoxHt]])
POLYFILL, edge, COLOR=!KON.Colors.white, THICK=1, /DEVICE

edge = TRANSPOSE( $
   [[0, 0, MapParams[iGeoRgn].map_xsize-1, MapParams[iGeoRgn].map_xsize-1, 0], $
    [0, MapParams[iGeoRgn].map_ysize-1, MapParams[iGeoRgn].map_ysize-1, 0, 0]])
PLOTS, edge, COLOR=0, THICK=1, /DEVICE

edge = TRANSPOSE( $
   [[0, MapParams[iGeoRgn].map_xsize-1],[MapParams[iGeoRgn].map_ysize-TextBoxHt, $
                                         MapParams[iGeoRgn].map_ysize-TextBoxHt]])
PLOTS, edge, COLOR=0, THICK=1, /DEVICE

edge = 0

END  ;  DrawBaseMap

;***************************************************************************
PRO DrawGeoRgn, NumGeoRgns, PolyPtr, GeoColors, PlumeParams, MapWindow
;***************************************************************************
; Draw the geographic region outline that the plume is found in.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get coordinates for the particular geographic region this plume belongs to
; and draw it over the base map.
;---------------------------------------------------------------------------

geo_coords = ROTATE(*PolyPtr[PlumeParams.georgn], 5)

PLOTS, geo_coords, LINESTYLE=0, COLOR=!KON.Colors.brown, THICK=2, /DATA

geo_coords = 0

END  ;  DrawGeoRgn

;***************************************************************************
PRO GetGeoRegionParams, NumGeoRgn, poly_ptr, rgn_name, rgn_colr, GeoRgnParams
;***************************************************************************
; Create and fill an array of data structures, each containing a description
; of a geographic region into which plume data will be partitioned. The last
; parameter is an alternative to defining maps for drawing all the regions.
; It's currently not used.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize the names of the geographic regions and the lat/lon coords of
; the map polygons defining them. Seven regions have been defined: Africa,
; Australia, SouthwestEurasia, NorthAmerica, BorealEurasia, SouthAmerica
; and SouthAsia. The final region is the whole globe to capture plumes that
; for any reason don't get assigned to a smaller region.
;---------------------------------------------------------------------------

rgn_name = ['Africa', 'Australia', 'SouthwestEurasia', 'NorthAmerica', $
            'BorealEurasia', 'SouthAmerica', 'SouthAsia']
   
rgn_colr = ['blue', 'magenta', 'brown', 'red', 'aqua', 'green', 'black']
   
IF (N_ELEMENTS(rgn_name) NE NumGeoRgn OR $
    N_ELEMENTS(rgn_colr) NE NumGeoRgn) THEN BEGIN
   mssg = 'The number of declared regions does not match the number of names.'
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   RETURN
ENDIF

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
   
;---------------------------------------------------------------------------
; Initialize the parameters for displaying individual regional maps. The map
; projection for individual regions is Lambert. For the world it is Robinson.
;---------------------------------------------------------------------------
;              Afr   Aust  WEurAs   NAm  BEurAs   SAm    SAs    Glob
lon_min  = [ -18.0, 105.0, -28.0,-168.0,  45.0, -85.0,  20.0, -180.0]
lon_max  = [  53.0, 180.0,  78.0, -60.0, 155.0, -30.0, 165.0,  180.0]
lat_min  = [ -36.0, -50.0,  33.0,   1.0,  30.0, -60.0, -20.0,  -85.0]
lat_max  = [  38.0,   7.0,  74.0,  71.0,  80.0,  15.0,  50.0,   85.0]
lon_cntr = [  20.0, 146.0,  19.0,-110.0, 112.0, -62.0, 104.0,    0.0]
lat_cntr = [   1.0, -22.0,  57.0,  45.0,  63.0, -20.0,  22.0,    0.0]
map_scal = [ 2.0E8, 1.5E8, 1.2E8, 1.9E8,1.15E8, 2.0E8, 2.3E8,  5.4E8]
ll_delta = [  10.0,  10.0,  10.0,  10.0,  10.0,  10.0,  10.0,   20.0]
lon_labl = [  -5.0, 150.0,  30.0,-130.0, 110.0, -30.0, 150.0,  -30.0]
lat_labl = [  -5.0, -15.0,  65.0,  15.0,  65.0, -35.0,  35.0,  -40.0]
coast_rs = [     1,     1,     1,     1,     1,     1,     1,      1]
map_xsiz = [  1000,  1000,  1000,  1000,  1000,  1000,  1000,   1570]
map_ysiz = [   700,   700,   700,   700,   700,   700,   700,    810]
xmarginL = [   2.5,   2.5,   2.5,   2.5,   2.5,   2.5,   2.5,    0.0]
xmarginR = [   2.5,   2.5,   2.5,   2.5,   2.5,   2.5,   2.5,    0.0]
ymarginB = [   0.4,   0.4,   0.4,   0.4,   0.4,   0.4,   0.4,    0.0]
ymarginT = [   1.7,   1.7,   1.7,   1.7,   1.7,   1.7,   1.7,    0.0]
map_proj = [     1,     1,     1,     1,     1,     1,     1,      2]

;---------------------------------------------------------------------------
; Create and return a data structure containing parameters describing all
; geographic regions.
;---------------------------------------------------------------------------

GeoRgnParams = { $
   map_title  : rgn_name, $
   color      : rgn_colr, $
   latlon     : poly_ptr, $
   lon_min    : lon_min,  $
   lon_max    : lon_max,  $
   lat_min    : lat_min,  $
   lat_max    : lat_max,  $
   lon_center : lon_cntr, $
   lat_center : lat_cntr, $
   map_scale  : map_scal, $
   ll_delta   : ll_delta, $
   lon_label  : lon_labl, $
   lat_label  : lat_labl, $
   coast_res  : coast_rs, $
   map_xsize  : map_xsiz, $
   map_ysize  : map_ysiz, $
   xmarginL   : xmarginL, $
   xmarginR   : xmarginR, $
   ymarginB   : ymarginB, $
   ymarginT   : ymarginT, $
   map_proj   : map_proj  $
}

lon_min  = 0
lon_max  = 0
lat_min  = 0
lat_max  = 0
lon_cntr = 0
lat_cntr = 0
map_scal = 0
ll_delta = 0
lon_labl = 0
lat_labl = 0
coast_rs = 0
map_xsiz = 0
map_ysiz = 0
xmarginL = 0
xmarginR = 0
ymarginB = 0
ymarginT = 0
map_proj = 0

END  ;  GetGeoRegionParams

;***************************************************************************
PRO DrawBlocks, MapParams, PlumeParams, MapWindow
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

NumBlocks = 5

;---------------------------------------------------------------------------
; Get the lat/long coordinates for a few blocks around the plume in this path.
;---------------------------------------------------------------------------

beg_block = (PlumeParams.block - NumBlocks / 2) > 1
end_block = (PlumeParams.block + NumBlocks / 2) < 180
NumBlocks = end_block - beg_block + 1

GetPathLatLonCoords, PlumeParams.path, beg_block, end_block, -180.0, LatLonCoords

;---------------------------------------------------------------------------
; Transfer the lat/lon values to the working arrays.
;---------------------------------------------------------------------------

lons = FLTARR(NumBlocks, 4)
lats = FLTARR(NumBlocks, 4)

FOR iblk = 0,NumBlocks-1  DO BEGIN
   FOR ipt1=0,3 DO BEGIN
      ipt2 = ipt1 * 2
      lons[iblk,ipt1] = LatLonCoords[ipt2,iblk]
      lats[iblk,ipt1] = LatLonCoords[ipt2+1,iblk]
   ENDFOR
ENDFOR

LatLonCoords = 0

;---------------------------------------------------------------------------
; Create any arrays needed below before entering loop.
;---------------------------------------------------------------------------

lonlatdat = FLTARR(2,4)
lonlatsav = FLTARR(2,2)

FOR iblk=0,NumBlocks-1 DO BEGIN
   ;------------------------------------------------------------------------
   ; Set up the current block's coordinates and draw the block outlines.
   ;------------------------------------------------------------------------
   
   lonlatdat[0,*] = lons[iblk,*]
   lonlatdat[1,*] = lats[iblk,*]

   lonlatsav[0:1,0] = lonlatdat[0:1,0]
   lonlatsav[0:1,1] = lonlatdat[0:1,3]
   
   PLOTS, lonlatdat, LINESTYLE=0, COLOR=!KON.Colors.black, THICK=1, /DATA
   PLOTS, lonlatsav, LINESTYLE=0, COLOR=!KON.Colors.black, THICK=1, /DATA
ENDFOR

lons = 0
lats = 0
lonlatdat = 0   
lonlatsav = 0
  
END  ;  DrawBlocks

;***************************************************************************
PRO DrawPoints, PlumeParams, MapWindow
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Draw the plume or cloud point.
;---------------------------------------------------------------------------

pt_color = (PlumeParams.type EQ 'plume') ? !KON.Colors.red : !KON.Colors.blue

PLOTS, [PlumeParams.lon, PlumeParams.lat], PSYM=6, COLOR=pt_color, THICK=3, $
       SYMSIZE=0.35, /DATA

END  ;  DrawPoints

;***************************************************************************
PRO CopyAndSave, BaseDir, PlumeParams, MapWindow
;***************************************************************************
; Make a copy of the map image and copy it to the correct directory and give
; it an appropriate name.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Construct the path name.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(PlumeParams.orbit),2)
name_len = STRLEN(orbit_str)
IF (name_len EQ 4) THEN orbit_str = '00' + orbit_str
IF (name_len EQ 5) THEN orbit_str =  '0' + orbit_str

outpath = BaseDir + orbit_str + !KON.Misc.Slash

IF (~ FILE_TEST(outpath, /DIRECTORY)) THEN BEGIN
   FILE_MKDIR, outpath
   rtrn_val = ChmodCatchError(outpath, '777'O)
ENDIF
       
outpath += PlumeParams.name + '_LocationMap.png'
   
;---------------------------------------------------------------------------
; Capture the image from the screen and write it to file.
;---------------------------------------------------------------------------

image = TVRD(/ORDER, TRUE=1)

WRITE_PNG, outpath, image, /ORDER
rtrn_val = ChmodCatchError(outpath, '666'O)

image = 0
   
END  ;  CopyAndSave

;***************************************************************************
PRO ListPlumeProjects_eh, event
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the data structure stored in the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, GET_UVALUE=proj_list_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   !SAV.Util.ProjlistIndex = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Branch to the widget control that was clicked.
;---------------------------------------------------------------------------

CASE 1 OF

   event.id EQ proj_list_struct.projlist : BEGIN
      !SAV.Util.ProjlistIndex = event.Index
   END
   event.id EQ proj_list_struct.ok_button : BEGIN
      IF (!SAV.Util.ProjlistIndex LT 0) THEN BEGIN
         mssg = 'You must select a project. Try again.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF ELSE BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         RETURN
      ENDELSE
   END
   event.id EQ proj_list_struct.cancel_button : BEGIN
      !SAV.Util.ProjlistIndex = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END
   
   event.id EQ proj_list_struct.help_button : BEGIN
      ;      GetDialogMessage, 5, HelpMsg
      res = DIALOG_MESSAGE(HelpMsg, /INFORMATION, /CENTER)
   END
   
ENDCASE

;---------------------------------------------------------------------------
; Save data structure back into the widget.
;---------------------------------------------------------------------------

WIDGET_CONTROL, event.top, SET_UVALUE=proj_list_struct, /NO_COPY
   
END ; ListPlumeProjects_eh

;***************************************************************************
PRO ListPlumeProjects_gui, ProjectList, OrblistIndex
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Define controls in widget.
;---------------------------------------------------------------------------

nproj = N_ELEMENTS(ProjectList)
nlines = nproj < 30

base0 = WIDGET_BASE(/COLUMN, TITLE='Select Plume Project', $
                    /TLB_KILL_REQUEST_EVENTS)
listname2 = WIDGET_LABEL(base0, VALUE='')
listname3 = WIDGET_LABEL(base0, /ALIGN_CENTER, YSIZE=20, $
                         VALUE='Select Project to Process')
projlist = WIDGET_LIST(base0, FRAME=1, VALUE=ProjectList, YSIZE=nlines)

base2 = WIDGET_BASE(base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON(base2, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON(base2, VALUE='Cancel')
help_button = WIDGET_BUTTON( base2, VALUE='Help' )

WIDGET_CONTROL, projlist, SET_LIST_SELECT=0
!SAV.Util.ProjlistIndex = 0

;---------------------------------------------------------------------------
; Define structure to be stored in widget.
;---------------------------------------------------------------------------

proj_list_struct = { $
   projlist:projlist, $
   ok_button:ok_button, $
   cancel_button:cancel_button, $
   help_button:help_button }
   
;---------------------------------------------------------------------------
; Store the structure in widget and realize it.
;---------------------------------------------------------------------------

WIDGET_CONTROL, base0, SET_UVALUE=proj_list_struct, XOFFSET=700, YOFFSET=250, $
                /NO_COPY
   
WIDGET_CONTROL, base0, /REALIZE

XMANAGER, 'ListPlumeProjects_gui', base0, EVENT_HANDLER='ListPlumeProjects_eh'

OrblistIndex = !SAV.Util.ProjlistIndex
   
END ; ListPlumeProjects_gui

;***************************************************************************
;***************************************************************************
PRO PlumeCreateWebFiles, DefaultProjDir
;***************************************************************************
;***************************************************************************
; Program reads all the raw plume text files created by MINX containing
; digitized plume and cloud points. Statistics are generated, and the final
; project summary databases are created that contain one line of data per
; plume or cloud. A location map for each plume is also created.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Select project to process.
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

RetrievePlumeProjects, project_data

ListPlumeProjects_gui, project_data[*].SecLevelDir, proj_num

IF (proj_num NE project_data[proj_num].Code)  THEN BEGIN
   mssg = ['The project code number does not match the record number', $
           'in the data structure for record ' + $
           STRTRIM(STRING(proj_num),2) + '. Quitting.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDIF

project_dir_name = (project_data[proj_num].SecLevelDir EQ 'NA') ? $
                    project_data[proj_num].TopLevelDir : $
                    project_data[proj_num].TopLevelDir + !KON.Misc.Slash + $
                    project_data[proj_num].SecLevelDir
   
;---------------------------------------------------------------------------
; Construct the names of the files to use for I/O.
;---------------------------------------------------------------------------

in_base_dir  = !KON.Misc.Slash + 'data' + !KON.Misc.Slash + 'plume' + $
               !KON.Misc.Slash + 'WebResults' + !KON.Misc.Slash + $
               project_dir_name + !KON.Misc.Slash

out_map_dir = in_base_dir

out_summary_dir = !SAV.WorkingDir + 'WebsitePlumeProjectSummary' + $
                  !KON.Misc.Slash

IF (~ FILE_TEST(out_summary_dir, /DIRECTORY)) THEN BEGIN
   FILE_MKDIR, out_summary_dir
   rtrn_val = ChmodCatchError(out_summary_dir, '777'O)
ENDIF

out_plume_file = out_summary_dir + project_data[proj_num].SecLevelDir + $
                 '_PlumeDatabase.txt'

out_save_file = out_summary_dir + project_data[proj_num].SecLevelDir + $
                '_ProjectVariables.sav'
                 
out_temp_file = out_summary_dir + project_data[proj_num].SecLevelDir + $
                '_LastOrbitProcessed.txt'

;***************************************************************************
; Ask the user what orbit to begin with. This is a clumsy work-around to
; allow restarting map-generation from any point when it has failed. The
; source of failure is in the MAP_SET or MAP_CONTINENTS procedure! You
; should enter 0 here the first time through to indicate 'do all' and create
; the summary file. If the task crashes later in MakePlumeLocationMaps, you
; can rerun this with a non-0 orbit number to continue the map generation
; process (and not regenerate the summary file etc.). Repeat until finished.
;---------------------------------------------------------------------------
FirstOrbit = 0
PRINT, 'Enter the orbit number you would like to begin with, e.g. 12345.'
PRINT, 'To create the summary file and attempt to generate all maps, enter 0.'
READ,  FirstOrbit
;***************************************************************************

;---------------------------------------------------------------------------
; If the user entered 0 as the first orbit to process, then process the
; plume data and save to file the variables needed for the last 2 procedures
; in case the program hangs or crashes. This saves repeating the time-
; consuming steps above. The program proceeds to generate as many maps as
; possible before it hangs.
; If the user did not enter 0 as the first orbit index, this code assumes
; the pertinent data have been saved to file, and it reads them and proceeds
; to generate maps beginning with the orbit entered (where hang occurred).
;---------------------------------------------------------------------------

IF (FirstOrbit EQ 0) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Read the plume data and store in an array of structures named data_array.
   ;------------------------------------------------------------------------
   
   ProcessRegionData, in_base_dir, num_out_fields, num_plumes, data_array, $
                      status
      
   ;------------------------------------------------------------------------
   ; Generate statistics for the plume data from the raw data tables.
   ;------------------------------------------------------------------------
   
   GenerateRegionStats, num_plumes, data_array, status
   
   ;------------------------------------------------------------------------
   ; Write plume results to project summary (ASCII) file and save variables.
   ;------------------------------------------------------------------------

   WriteRegionDatabase, project_data[proj_num], out_summary_dir, out_plume_file, $
                        num_out_fields, num_plumes, data_array, status
                        
   SAVE, FILENAME=out_save_file, project_data, num_plumes, data_array

ENDIF ELSE BEGIN
   
   ;------------------------------------------------------------------------
   ; Restore variables.
   ;------------------------------------------------------------------------

   RESTORE, FILENAME=out_save_file
   
ENDELSE

;---------------------------------------------------------------------------
; Collect lat/long coords, names etc. for creation of location map display.
;---------------------------------------------------------------------------

CollectMapData, num_plumes, data_array, plume_array, num_geo_rgns, map_params, $
                txt_box_ht
data_array = 0

;---------------------------------------------------------------------------
; Generate the location maps and save to web directory.
;---------------------------------------------------------------------------

MakePlumeLocationMaps, project_data[proj_num], out_map_dir, num_plumes,$
                       plume_array, num_geo_rgns, map_params, txt_box_ht, $
                       FirstOrbit, out_temp_file
plume_array = 0
map_params = 0
project_data = 0

rtrn = DIALOG_MESSAGE('Completed processing', /CENTER, /INFORMATION)

END  ;  PlumeCreateWebFiles
