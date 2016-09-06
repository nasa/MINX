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
PRO GetPixNdxList, Thresh, NumPix, DistMtrx, UnassignAry, ThisRow, NewList
;***************************************************************************
; Recursive routine to get a list of all fire pixels within Thresh km of any
; fire pixel in a fire.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the list of unassigned indices within Thresh km of the current fire
; pixel.  Exclude any new entries that have already been found.
;---------------------------------------------------------------------------

IF (ThisRow LT NumPix-1) THEN BEGIN
   dist_list = DistMtrx[ThisRow,*]
   this_list = WHERE(dist_list[ThisRow+1:NumPix-1] LT Thresh, numdist)
   dist_list = 0

   IF (numdist GT 0) THEN BEGIN
      this_list += ThisRow + 1

      FOR ipix=0,N_ELEMENTS(this_list)-1 DO BEGIN
         IF (UnassignAry[this_list[ipix]] GE 0) THEN BEGIN

            GetPixNdxList, Thresh, NumPix, DistMtrx, UnassignAry, $
                           this_list[ipix], NewList
         ENDIF
      ENDFOR
   ENDIF
ENDIF

NewList = [NewList, ThisRow]
NewList = NewList[UNIQ(NewList, SORT(NewList))]
UnassignAry[ThisRow] = FIX(!KON.Misc.BADVALUE_REAL)

END  ;  GetPixNdxList

;***************************************************************************
PRO ClusterFirePix, LatList, LonList, BlkList, LineList, SampList, Orbit, $
                    Path, Date, Time, NumTok, FireList, NumFire
;***************************************************************************
; Cluster the fire pixels into potential fire events by finding the fire
; pixel nearest the center of each cluster and reporting it. A threshold
; distance is used to determine whether a fire pixel belongs to a fire.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Set a threshold distance in km.  All pixels within this distance from
; another pixel in a fire belong to that fire.
;---------------------------------------------------------------------------

DTHRESH = 5.0
MeanPower = -99.9
NumFire = 0

;---------------------------------------------------------------------------
; If there's only one pixel in this orbit, return it as a fire.
;---------------------------------------------------------------------------

IF (NumTok EQ 1) THEN BEGIN
   NumFire = 1
   FireList[0] = $
       STRING(FORMAT='(I5,I5,I5,I5,F9.3,F10.3,I5,A12,A12,I5,F8.2)', $
              Orbit, BlkList[0], LineList[0], SampList[0], $
              LatList[0], LonList[0], Path, Date, Time, NumFire, MeanPower)
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Get the lat/long values and project all orthographically to x/y.
;---------------------------------------------------------------------------

map_struct = MAP_PROJ_INIT('Orthographic', CENTER_LONGITUDE=LonList[0], $
                           CENTER_LATITUDE=LatList[0])

xy_pts = MAP_PROJ_FORWARD(LonList, LatList, MAP_STRUCTURE=map_struct) * 0.001

;---------------------------------------------------------------------------
; Construct a matrix of distances from all points to all other points.
;---------------------------------------------------------------------------

dist_mtrx = DISTANCE_MEASURE(xy_pts, /MATRIX)

;---------------------------------------------------------------------------
; Use a clustering algorithm to distribute the pixels into clusters.
; Continue looping while there remain unassigned pixels.
;---------------------------------------------------------------------------

unassign_pix_ary = INDGEN(NumTok)
num_unassign_pix = NumTok
pix_ary = INDGEN(NumTok)

WHILE (num_unassign_pix GT 0) DO BEGIN

   unassign_ndx = WHERE(unassign_pix_ary GE 0, count_ndx)
   IF (count_ndx EQ 0) THEN BREAK

   ;------------------------------------------------------------------------
   ; Loop over all fire pixels starting with the next unassigned one. For
   ; the current pixel, recursively get the indices of all fire pixels that 
   ;are within DTHRESH of any previously retrieved pixel.
   ;------------------------------------------------------------------------

   new_list = INTARR(1) - 1

   GetPixNdxList, DTHRESH, NumTok, dist_mtrx, unassign_pix_ary, $
                  unassign_ndx[0], new_list

   num_new = N_ELEMENTS(new_list) - 1
   new_list = new_list[1:num_new]

   ;------------------------------------------------------------------------
   ; Find the fire pixel nearest the center of this fire cluster.
   ;------------------------------------------------------------------------

   sumx = 0.0D
   sumy = 0.0D

   FOR ipix=0,num_new-1 DO BEGIN
      sumx += xy_pts[0, pix_ary[new_list[ipix]]]
      sumy += xy_pts[1, pix_ary[new_list[ipix]]]
   ENDFOR

   x_mid = sumx / num_new
   y_mid = sumy / num_new
   near_dist = DOUBLE(!KON.Misc.LARGE_POS_NUM * 10.0)
   near_ndx = -1

   nndx = pix_ary[new_list[0]]

   FOR ipix=0,num_new-1 DO BEGIN
      dist = SQRT((x_mid - xy_pts[0, pix_ary[new_list[ipix]]]) * $
                  (x_mid - xy_pts[0, pix_ary[new_list[ipix]]]) + $
                  (y_mid - xy_pts[1, pix_ary[new_list[ipix]]]) * $
                  (y_mid - xy_pts[1, pix_ary[new_list[ipix]]]))
      IF (dist LT near_dist) THEN BEGIN
         near_dist = dist
         nndx = pix_ary[new_list[ipix]]
      ENDIF
   ENDFOR

   ;------------------------------------------------------------------------
   ; Update the array of unassigned pixels and save latest fire point.
   ;------------------------------------------------------------------------

   IF (num_new GE 0) THEN num_unassign_pix -= num_new

   FireList[NumFire] = $
            STRING(FORMAT='(I5,I5,I5,I5,F9.3,F10.3,I5,A12,A12,I5,F8.2)', $
            Orbit,  BlkList[nndx],  LineList[nndx], SampList[nndx], $
            LatList[nndx],  LonList[nndx], Path, Date, Time, num_new, MeanPower)

   NumFire += 1
   new_list = 0

ENDWHILE

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

lat_flt = 0
lon_flt = 0
xy_pts = 0
dist_mtrx = 0
unassign_ndx = 0
unassign_pix_ary = 0
pix_ary = 0

END ; ClusterFirePix

;***************************************************************************
PRO BreakBlkListForOrbit, BlkList, NumBlock, BlkBegList, BlkEndList, $
                          NumGroup, Status
;***************************************************************************
; Function takes a list of blocks and breaks them into one or more groups of
; blocks that are no larger than the largest number that the user's computer
; can handle at a time in MINX. Each group must have an extra block added at
; the beginning and the end, and these must fit in the maximum size as well.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Create the arrays of output block begin and end numbers.
;---------------------------------------------------------------------------

BlkBegList = INTARR(NumBlock)
BlkEndList = INTARR(NumBlock)
NumElem    = N_ELEMENTS(BlkList)
NumGroup   = 0

;---------------------------------------------------------------------------
; Create a working block-group array and split the blocks into groups
; separated by blocks with (ideally) no fire pixels.
;---------------------------------------------------------------------------

igrp = 0
BlkBegList[igrp] = (BlkList[0] - 1) > 1

FOR iblk=0,NumElem-1 DO BEGIN

   IF ((BlkList[iblk] + 1) GT $
       (BlkBegList[igrp] + !SAV.Util.MaxBlksPerLoad - 1)) THEN BEGIN

      BlkEndList[igrp] = BlkList[iblk-1] + 1
      igrp += 1

      IF (BlkList[iblk] EQ BlkList[iblk-1] + 1) THEN BEGIN
         BlkBegList[igrp] = (BlkList[iblk]) > 1
      ENDIF ELSE BEGIN
         BlkBegList[igrp] = (BlkList[iblk] - 1) > 1
      ENDELSE
   ENDIF

ENDFOR

BlkEndList[igrp] = (BlkList[NumBlock-1] + 1) < !KON.Instr.NUM_BLOCKS
NumGroup = igrp + 1

;---------------------------------------------------------------------------
; Chop off unused array elements and make sure we're not outside the range
; of valid blocks.
;---------------------------------------------------------------------------

BlkBegList = BlkBegList[0:NumGroup-1]
BlkEndList = BlkEndList[0:NumGroup-1]

Status = 0

END  ;  BreakBlkListForOrbit

;***************************************************************************
PRO InsertHeaderRecords, IOUnit, L1B2_Dir, L1B2_Ver, Output_Dir
;***************************************************************************
; Function writes the 3 header records needed by the orbit processing list
; file. It gets them by asking user for them in a dialog box.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Get the header lines from the user in a gui.
;---------------------------------------------------------------------------

Header1 = L1B2_Dir
Header2 = L1B2_Ver
Header3 = Output_Dir

InsertHeaderRecords_gui, Header1, Header2, Header3, Status
IF (Status NE 0) THEN RETURN

;---------------------------------------------------------------------------
; Write the records to the top of the file.
;---------------------------------------------------------------------------

PRINTF, IOUnit, Header1
PRINTF, IOUnit, Header2
PRINTF, IOUnit, Header3

;---------------------------------------------------------------------------
; Save the new directory location for the L1B2 files in the !VAR variable.
;---------------------------------------------------------------------------

!SAV.DfltFiles[!KON.FileTyp.TypeL1B2Terrain].SavePath = Header1

END  ;  InsertHeaderRecords

;***************************************************************************
PRO ProcessModVolcHotSpots, DefaultProjDir
;***************************************************************************
; This is option 1 of 2 options for creating fire pixel data for a plume
; project. See ProcessModisFirePixels below for the preferred method. Both
; methods end up requiring the download of MODIS granules and running the
; ProcessModisFirePixels procedure if FRP data are needed. This option may
; not provide as complete/accurate a collection of fire pixels, but it
; requires downloading far fewer MODIS granules. If FRP data aren't needed,
; just fire locations, then this option can do that without the need to
; download MODIS granules.
; 
; Program reads a file you extract from the ModVolc website that contains a
; summary of all fire pixels (one per record, thus the data volume savings)
; in a specified rectangular geographic area for a specified time interval.
; As of February, 2015, the ModVolc website was http://modis.higp.hawaii.edu/.
; It then filters out Aqua pixels (retaining only Terra) as well as pixels
; that are not in the desired time or MISR block ranges. Remaining pixels
; are sorted first by date and then by latitude. Next, fire pixels are
; compressed into clusters called fire events, which are indexed by a lat/
; lon near the center of the cluster. The date and lat/lon of each fire
; event are then used to find the MISR orbit and block it falls in. Final
; results are written to three files in ASCII formats that MINX etc. can
; read: 1) the first contains a list of the MISR orbits and block ranges
; where thermal anomalies were found. This will be used for input to MINX
; to be used in the selection of orbits to process using main menu option
; "Process Plume Projects"; 2) another contains a comma-separated list of
; orbit numbers that can be used to retrieve MISR products using the MISR
; order tool at the DAAC; 3) the last contains a list of MODIS granules
; that can be used to order those products. The user can opt to let MINX
; immediately download the MODIS granules using FTP.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; Initialize some parameters.
;---------------------------------------------------------------------------

UseModisFirePix = 1      ; Set to 1 to create fire pixels by reading MODIS
                         ; MOD14 files. Set to 0 to use ModVolc pixels, in
                         ; which case there is no FRP data. Default is to
                         ; use MODIS - this is required for Plume Projects.
ShowRejections = 0       ; Set to 1 - 6 to show a message whenever a pixel
                         ; is eliminated for one of 6 reasons. For testing.
                         ; 99 = show for all reasons; 0 = don't show any

MAX_PLUMES = 25000L      

;---------------------------------------------------------------------------
; Ask the user for some parameters: the first and last MISR block numbers to
; consider valid for these data, the number of fire pixels there must be in
; a MISR orbit to use that orbit, the name of the project for adding to the
; file name and whether the user wants MODIS power, temp and fire pixels,
; or if the ModVolc fire pixels are adequate.
;---------------------------------------------------------------------------

PlumeProjectName = !SAV.Util.ProjectName
FirstValidBlock  = !SAV.Util.FirstValidBlock
LastValidBlock   = !SAV.Util.LastValidBlock
BegDate          = !SAV.Util.BegDate
EndDate          = !SAV.Util.EndDate
MaxBlksPerLoad   = !SAV.Util.MaxBlksPerLoad
NumFirePixOrbit  = !SAV.Util.NumFirePixOrbit
UseModisFirePix  = !SAV.Util.FirePixGetMethod

GetModvolcParams_gui, PlumeProjectName, FirstValidBlock, LastValidBlock, $
                      BegDate, EndDate, UseModisFirePix, MaxBlksPerLoad, $
                      NumFirePixOrbit, cancel
IF (cancel EQ 1) THEN RETURN

;---------------------------------------------------------------------------
; Select the input file containing the ModVolc fire pixels that the user
; downloaded - it must already exist. The host directory will become the
; default project directory for the remaining directory selections. Also
; set it in the !VAR variable.
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypeModvolc, '*.txt', 0, DefaultProjDir, $
                 ModvolcListFile
IF (ModvolcListFile EQ '') THEN RETURN

!SAV.DfltFiles[!KON.FileTyp.TypeUtilProjDir].SavePath = DefaultProjDir

;---------------------------------------------------------------------------
; Test that the file is of the correct type.
;---------------------------------------------------------------------------

!ERROR_STATE.CODE = 0
ON_IOERROR, bad_format1
bad_format1:
IF (!ERROR_STATE.CODE NE 0) THEN BEGIN
   mssg = ['File ' + ModvolcListFile, $
           'may have the wrong format for a Modvolc fire pixel file.', $
           'Look for the correct file or fix the contents of this one.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   !ERROR_STATE.CODE = 0
   ON_IOERROR, NULL
   RETURN
ENDIF

buff = ''
OPENR, Unit0, ModvolcListFile, /GET_LUN
BegMidEnd = 0
ReadNextAsciiLine, Unit0, BegMidEnd, buff
FREE_LUN, Unit0
toks = STRSPLIT(buff, ' ', COUNT=numtok, /EXTRACT)
IF (numtok LT 22) THEN BEGIN
   !ERROR_STATE.CODE = -1
   GOTO, bad_format1
ENDIF
nlen0 = STRLEN(toks[0])
nlen1 = STRLEN(toks[1])
nlen2 = STRLEN(toks[2])
; Old check relies on ModVolc first field being UNIX timestamp but website is broken
; Old check was IF (nlen0 NE 10 OR nlen1 NE 1 OR nlen2 NE 4) THEN BEGIN
IF (nlen1 NE 1 OR nlen2 NE 4) THEN BEGIN
   !ERROR_STATE.CODE = -1
   GOTO, bad_format1
ENDIF
toks = 0
!ERROR_STATE.CODE = 0
ON_IOERROR, NULL

;---------------------------------------------------------------------------
; Then count the number of fire pixel records in the Modvolc file.
;---------------------------------------------------------------------------

num_fire_pixels = FILE_LINES(ModvolcListFile)
strNumPix = STRTRIM(STRING(num_fire_pixels),2)
         
;---------------------------------------------------------------------------
; Construct the names of the output report file, common to options 1 and 2.
;---------------------------------------------------------------------------

out_log_file = DefaultProjDir + 'FirePixReport_Modvolc_' + PlumeProjectName + $
               '.txt'
   
;---------------------------------------------------------------------------
; Construct some file names for the option that uses no MODIS data. One
; output fire pixel file is created for each orbit, so just make the suffix.
; The "__" will be replaced later with "_<orbit>_".
;---------------------------------------------------------------------------
   
IF (UseModisFirePix EQ 0) THEN BEGIN
   out_orbit_file = DefaultProjDir + 'MisrOrderList_Modvolc_' + $
                    PlumeProjectName + '.txt'
   out_process_file = DefaultProjDir + 'MisrProcessList_Modvolc_' + $
                      PlumeProjectName + '.txt'
   out_pixel_file  = 'FirePixels_Modvolc__' + PlumeProjectName + '.txt'
ENDIF

;---------------------------------------------------------------------------
; Construct the name of output file to contain downloaded MOD14 granule URLs.
;---------------------------------------------------------------------------
   
IF (UseModisFirePix EQ 1) THEN BEGIN
   out_URL_list_file = DefaultProjDir + 'MOD14_GranuleList_Modvolc_' + $
                       PlumeProjectName + '.txt'
                       
   !SAV.DfltFiles[!KON.FileTyp.TypeMOD14_URLfile].SavePath = out_URL_list_file
ENDIF

;---------------------------------------------------------------------------
; Create a message to user that processing will take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
                     TITLE='Processing .....', $
                     XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
                     YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ', '  MINX is processing ' + strNumPix + ' fire pixels.', $
        '  Please be patient ......................', ' ']
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=45, YSIZE=4)
WIDGET_CONTROL, /REALIZE, baseID  
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Create an array to hold the data, plus other initialization.
;---------------------------------------------------------------------------

filter_list = STRARR(num_fire_pixels)

buff             = ''
irec             = 1L
iline            = 1L
nlines_total     = 0L
nlines_terra     = 0L
nlines_aqua      = 0L
nlines_misr      = 0L
nlines_fire      = 0L
nlines_date      = 0L
nlines_badline   = 0L
nlines_badblock  = 0L
nlines_onepixel  = 0L
nlines_swathedge = 0L

;---------------------------------------------------------------------------
; Convert the beginning and ending dates to Julian.
;---------------------------------------------------------------------------

GetJulianDay, !SAV.Util.BegDate, '00:00:00', 0, ymd, hms, julian_beg
GetJulianDay, !SAV.Util.EndDate, '12:59:59', 0, ymd, hms, julian_end

;---------------------------------------------------------------------------
; Set up error handling to detect the end of file.
; Read the ModVolc data file. Skip the first header line and filter out the
; unwanted pixels: all AQUA pixels plus pixels not in selected date range.
;---------------------------------------------------------------------------

granule_5min = 0.00347222  ; Fraction of day that 5 minutes is for MOD14 granule.
BegMidEnd = 0
OPENR, unit, ModvolcListFile, /GET_LUN

FOR iline=0L,num_fire_pixels DO BEGIN

   ReadNextAsciiLine, unit, BegMidEnd, buff

   tok1 = STRSPLIT(buff, ' ', COUNT=ntok1, /EXTRACT)
   IF (ntok1 LT 9) THEN BEGIN
      IF (BegMidEnd EQ 3) THEN BREAK
      CONTINUE
   ENDIF
   nlines_total += 1
   
   sat  = tok1[1]
   year = tok1[2]
   mnth = tok1[3]
   day  = tok1[4]
   hour = tok1[5]
   min  = tok1[6]
   lon  = tok1[7]
   lat  = tok1[8]
   
   ;------------------------------------------------------------------------
   ;ModVolc times are supposed to be MODIS times, but they aren't
   ;so we fix up min to be previous 5 minute boundary
   ;------------------------------------------------------------------------
   min=FIX(min)
   IF min MOD 5 NE 0 THEN min = min - (min MOD 5)   
   min=STRING(min, FORMAT='(I2.2)')

   ;------------------------------------------------------------------------
   ; Convert the ModVolc date/time to Julian time and compare to the date
   ; range requested by the user. ModVolc times are simply MODIS granule
   ; times which are measured from the start of each 5 minute granule! So
   ; the error in determining block numbers from these times is about +/-
   ; 8 blocks. Consequently we must use times for the beginning and ending
   ; of each MODIS granule to do the filtering. Also filter out all Aqua
   ; (sat EQ 'A') fire pixels here.
   ;------------------------------------------------------------------------

   date0 = year + '-' + mnth + '-' + day
   time0 = hour + ':' + min  + ':00'   

   GetJulianDay, date0, time0, 0, ymd, hms, julian_day1
   julian_day2 = julian_day1 + granule_5min

   IF (sat EQ 'T' AND $
       ((julian_day1 LE julian_beg AND julian_day2 GE julian_beg) OR $
        (julian_day1 LE julian_end AND julian_day2 GE julian_end) OR $
        (julian_day1 LE julian_beg AND julian_day2 GE julian_end) OR $
        (julian_day1 GE julian_beg AND julian_day2 LE julian_end))) THEN BEGIN

      filter_list[nlines_terra] = date0 + ' ' + lat + ' ' + lon + ' ' + $
                                  'T' + time0 + 'Z'
      nlines_terra += 1L
   ENDIF ELSE BEGIN
      IF (sat EQ 'A') THEN BEGIN
         nlines_aqua += 1L
      ENDIF ELSE BEGIN
         nlines_date += 1L
      ENDELSE
   ENDELSE

   IF (BegMidEnd EQ 3) THEN BREAK
ENDFOR

FREE_LUN, unit

;---------------------------------------------------------------------------
; Find corresponding MISR orbit, path, block from the MODIS date and time.
; The format of datetime parameter is: 'YYYY-MM-DDThh:mm:ssZ'. Points that
; are on the night-side or are within the wide MODIS swath but not in the
; much narrower MISR swath are automatically eliminated.
;---------------------------------------------------------------------------

IF (nlines_terra LT 1) THEN BEGIN
   mssg = 'No valid fire pixels were found - recheck your input.'
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   WIDGET_CONTROL, baseID, /DESTROY
   RETURN
ENDIF

misr_filter_lats = FLTARR(nlines_terra)
misr_filter_lons = FLTARR(nlines_terra)
misr_filter_pths = INTARR(nlines_terra)
misr_filter_orbs = LONARR(nlines_terra)
misr_filter_blks = INTARR(nlines_terra)
misr_filter_lins = INTARR(nlines_terra)
misr_filter_smps = INTARR(nlines_terra)
misr_filter_dats = STRARR(nlines_terra)
misr_filter_tims = STRARR(nlines_terra)
misr_filter_list = STRARR(nlines_terra)

iline = 0L
orbit = -1L

FOR iline=0L,nlines_terra-1 DO BEGIN

   tok2 = STRSPLIT(filter_list[iline], ' ', COUNT=ntok2, /EXTRACT)

   ;------------------------------------------------------------------------
   ; Filter out ModVolc entries with too few fields.
   ;------------------------------------------------------------------------

   IF (ntok2 NE 4) THEN BEGIN
      nlines_badline += 1
      IF (ShowRejections EQ 1 OR ShowRejections EQ 99) THEN BEGIN
         mssg = 'ERROR - Number of tokens less than 4 for line ' + iline
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Use Get_OrbitTime_OrbitNumber to get two orbit numbers, one for the block
   ; beginning and one for the block end. Don't use the block number returned
   ; here. Instead use LatLonToBLS to get the block number.
   ;------------------------------------------------------------------------

   GetJulianDay, tok2[0], tok2[3], 0, ymd, hms, JulianTime1
   JulianTime2 = JulianTime1 + granule_5min

   orbit1 = -1L
   orbit2 = -1L
   block1 = -1
   block2 = -1
   Get_OrbitTime_OrbitNumber, JulianTime1, 0, orbit1, block1, status
   Get_OrbitTime_OrbitNumber, JulianTime2, 0, orbit2, block2, status
   
   IF (orbit1 NE orbit2) THEN BEGIN
      IF (orbit2 GT 0) THEN orbit = orbit2
      IF (orbit1 GT 0) THEN orbit = orbit1
   ENDIF ELSE BEGIN
      orbit = orbit1
   ENDELSE

   ;------------------------------------------------------------------------
   ; Get the MISR path.
   ;------------------------------------------------------------------------

   IF (orbit GT 0) THEN BEGIN
      path = PathFromOrbit(orbit)
   ENDIF ELSE BEGIN
      IF (ShowRejections EQ 2 OR ShowRejections EQ 99) THEN BEGIN
         datetime = tok2[0] + tok2[3]
         mssg = 'Date/Time ' + datetime + ' rejected for line ' + iline
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF
      CONTINUE
   ENDELSE

   ;------------------------------------------------------------------------
   ; Now get the block, line and sample from the lat-lon for the orbit and
   ; use them to filter out ModVolc entries not in the desired block range
   ; and not in the valid zones of MISR's swath edges.
   ;------------------------------------------------------------------------

   LatLonToBLS, path, DOUBLE(tok2[1]), DOUBLE(tok2[2]), block, line, samp

   IF (block LT !SAV.Util.FirstValidBlock OR $
       block GT !SAV.Util.LastValidBlock) THEN BEGIN
      nlines_badblock += 1
      IF (ShowRejections EQ 4 OR ShowRejections EQ 99) THEN BEGIN
         mssg = 'Block out of range on Orbit ' + STRTRIM(STRING(orbit),2) + $
                ' so rejected line ' + iline
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF
      CONTINUE
   ENDIF

   IF (samp LT !SAV.Util.FirstValidSamp OR $
       samp GT !SAV.Util.LastValidSamp) THEN BEGIN
      nlines_swathedge += 1
      IF (ShowRejections EQ 5 OR ShowRejections EQ 99) THEN BEGIN
         mssg = 'Fire pix on swath edge on Orbit ' + STRTRIM(STRING(orbit),2) + $
                ' so rejected for line ' + iline
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      ENDIF
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Create a list of fire pixels ordered orbit, block, line, sample, lat,
   ; long, date, time.
   ;------------------------------------------------------------------------

   misr_filter_lats[nlines_misr] = tok2[1]
   misr_filter_lons[nlines_misr] = tok2[2]
   misr_filter_pths[nlines_misr] = path
   misr_filter_orbs[nlines_misr] = orbit
   misr_filter_blks[nlines_misr] = block
   misr_filter_lins[nlines_misr] = ROUND(line)
   misr_filter_smps[nlines_misr] = ROUND(samp)
   misr_filter_dats[nlines_misr] = tok2[0]
   misr_filter_tims[nlines_misr] = tok2[3]

   misr_filter_list[nlines_misr] = $
                    STRING(orbit) + ' ' + STRING(block) + ' ' + $
                    STRING(ROUND(line))  + ' ' + STRING(ROUND(samp))  + ' ' + $
                    tok2[1] + ' ' + tok2[2] + ' ' + tok2[0] + ' ' + tok2[3]
   nlines_misr += 1L

ENDFOR

IF (nlines_misr EQ 0) THEN BEGIN
   mssg = 'There are no fire pixels that fall in any MISR orbit. Quitting.'
   rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   WIDGET_CONTROL, baseID, /DESTROY
   RETURN
ENDIF

filter_list = 0

;---------------------------------------------------------------------------
; Truncate the array. Then sort the remaining fire pixels by orbit, lat,
; lon... Reverse order so the latitudes will start from north (block order).
;---------------------------------------------------------------------------

misr_filter_list = misr_filter_list[0L:nlines_misr-1L]
sort_order = SORT(misr_filter_list)

misr_sort_lats = misr_filter_lats[sort_order[*]]
misr_sort_lons = misr_filter_lons[sort_order[*]]
misr_sort_orbs = misr_filter_orbs[sort_order[*]]
misr_sort_blks = misr_filter_blks[sort_order[*]]
misr_sort_lins = misr_filter_lins[sort_order[*]]
misr_sort_smps = misr_filter_smps[sort_order[*]]
misr_sort_pths = misr_filter_pths[sort_order[*]]
misr_sort_dats = misr_filter_dats[sort_order[*]]
misr_sort_tims = misr_filter_tims[sort_order[*]]

misr_filter_lats = 0
misr_filter_lons = 0
misr_filter_orbs = 0
misr_filter_blks = 0
misr_filter_lins = 0
misr_filter_smps = 0
misr_filter_pths = 0
misr_filter_dats = 0
misr_filter_tims = 0
misr_filter_list = 0

;---------------------------------------------------------------------------
; If requested, filter out all orbits that have fewer than a certain number
; of fire pixels.
;---------------------------------------------------------------------------

IF (!SAV.Util.NumFirePixOrbit GT 0) THEN BEGIN

   beg_ndx = 0L
   end_ndx = -1L
   uniq_orbits = misr_sort_orbs[UNIQ(misr_sort_orbs)]
   num_uniq = N_ELEMENTS(uniq_orbits)

   FOR iuniq=0,num_uniq-1 DO BEGIN

      ndxs = WHERE(misr_sort_orbs[0L:nlines_misr-1L] EQ $
                   uniq_orbits[iuniq], numndxs)

      IF (numndxs LT !SAV.Util.NumFirePixOrbit) THEN BEGIN
         nlines_onepixel += numndxs
         IF (ShowRejections EQ 6 OR ShowRejections EQ 99) THEN BEGIN
            mssg = 'Too few fire pixels on Orbit ' + $
                   STRTRIM(STRING(orbit),2) + $
                   ' rejected for line ' + iline
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         ENDIF
         CONTINUE
      ENDIF

      end_ndx += numndxs

      misr_sort_lats[beg_ndx:end_ndx] = misr_sort_lats[ndxs]
      misr_sort_lons[beg_ndx:end_ndx] = misr_sort_lons[ndxs]
      misr_sort_orbs[beg_ndx:end_ndx] = misr_sort_orbs[ndxs]
      misr_sort_blks[beg_ndx:end_ndx] = misr_sort_blks[ndxs]
      misr_sort_lins[beg_ndx:end_ndx] = misr_sort_lins[ndxs]
      misr_sort_smps[beg_ndx:end_ndx] = misr_sort_smps[ndxs]
      misr_sort_pths[beg_ndx:end_ndx] = misr_sort_pths[ndxs]
      misr_sort_dats[beg_ndx:end_ndx] = misr_sort_dats[ndxs]
      misr_sort_tims[beg_ndx:end_ndx] = misr_sort_tims[ndxs]

      beg_ndx += numndxs

   ENDFOR

   ndxs = 0

   nlines_misr = end_ndx + 1L

   IF (nlines_misr LT 1) THEN BEGIN
      mssg = 'No valid fire pixels were found on MISR orbits.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      WIDGET_CONTROL, baseID, /DESTROY
      RETURN
   ENDIF

ENDIF

;---------------------------------------------------------------------------
; Write the filtered, sorted MISR pixel locations to files, one orbit per
; file. Do only if user selected to use ModVolc fire pixels (no FRP).
;---------------------------------------------------------------------------

IF (UseModisFirePix EQ 0) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Let the user select the directory where output ModVolc fire pixels will
   ; be written. Create directory if needed. It's most convenient if the
   ; user creates a subdirectory in directory containing the ModVolc data.
   ;------------------------------------------------------------------------
   
   !SAV.DfltFiles[!KON.FileTyp.TypeFireDir].SavePath = DefaultProjDir + $
            'FirePixels_Modvolc_' + !SAV.Util.ProjectName + !KON.Misc.Slash
      
   GetLastFilename, 0, !KON.FileTyp.TypeFireDir, '*', 0, FirePixelSubdir, $
                    dummy
   IF (FirePixelSubdir EQ '') THEN BEGIN
      WIDGET_CONTROL, baseID, /DESTROY
      RETURN
   ENDIF
   
   IF (~ FILE_TEST(FirePixelSubdir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(FirePixelSubdir)
      rtrn_val = ChmodCatchError(FirePixelSubdir, '777'O)
   ENDIF

   prev_orbit = 0L
   irec = 0L

   FOR irec=0L,nlines_misr-1L DO BEGIN

      orbit = misr_sort_orbs[irec]
      path  = misr_sort_pths[irec]
      date1 = misr_sort_dats[irec]
      time1 = misr_sort_tims[irec]

      IF (prev_orbit NE 0L AND orbit NE prev_orbit) THEN FREE_LUN, unit

      ;---------------------------------------------------------------------
      ; If it's a new orbit, open new file and write header to pixel file.
      ;---------------------------------------------------------------------

      IF (orbit NE prev_orbit) THEN BEGIN
         IF (orbit LE 99999) THEN orbit0 = '0'  + STRTRIM(STRING(orbit),2)
         IF (orbit LE  9999) THEN orbit0 = '00' + STRTRIM(STRING(orbit),2)

         toks = STRSPLIT(out_pixel_file, '__', /REGEX, /EXTRACT, COUNT=numtoks)
         IF (numtoks NE 2) THEN BEGIN
            mssg = ['Could not add orbit number to filename string:', $
                    out_pixel_file, 'Quitting.']
            rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
            WIDGET_CONTROL, baseID, /DESTROY
            RETURN
         ENDIF

         outpath_pixels = FirePixelSubdir + toks[0] + '_' + orbit0 + '_' + toks[1]
         
         OPENW, unit, outpath_pixels, /GET_LUN
         descr_str = 'Fire pixels from ModVolc database on 275m MISR ' + $
                     'SOM grid for project : ' + PlumeProjectName
         PRINTF, unit, descr_str
         PRINTF, unit, STRTRIM(STRING(orbit),2) + ' / ' + $
                       STRTRIM(STRING(path), 2) + ' / ' + date1 + $
                       '  : orbit / path / date'
         PRINTF, unit, ' Longitude  Latitude Blk Samp Line'
         PRINTF, unit, '  degrees    degrees      0-based'
      ENDIF

      ;---------------------------------------------------------------------
      ; Write the next pixel data to its file.
      ;---------------------------------------------------------------------

      PRINTF, unit, FORMAT='(2F10.5,I4,I5,I5)', $
                    misr_sort_lons[irec], misr_sort_lats[irec], $
                    misr_sort_blks[irec], misr_sort_smps[irec], $
                    misr_sort_lins[irec]

      prev_orbit = orbit
   ENDFOR

   FREE_LUN, unit
   rtrn_val = ChmodCatchError(outpath_pixels, '666'O)

   ;------------------------------------------------------------------------
   ; Set up to write MISR files: orbit process list and orbit order list.
   ;------------------------------------------------------------------------

   OPENW, unit1, out_process_file, /GET_LUN
   InsertHeaderRecords, unit1, $
                        !SAV.DfltFiles[!KON.FileTyp.TypeL1B2Terrain].SavePath, $
                        'F03_0024', DefaultProjDir + 'Digitize_Output' + $
                        !KON.Misc.Slash
   OPENW, unit2, out_orbit_file, /GET_LUN

   ;------------------------------------------------------------------------
   ; Write a file appropriate for input to MINX with orbit, first block and
   ; last block. Break each orbit into block chunks manageable by user's MINX
   ; installation as needed. Add 1 block before each first and after each last
   ; block in each chunk. Do only if user selected to use ModVolc fire pixels.
   ;------------------------------------------------------------------------
   
   end_ndx = 0L
   prev_orbit = 0L
   rec_done = 0
   num_orbit = 0
   irec = 0L
   
   WHILE (1) DO BEGIN   ;  loop over pixels looking for orbits
   
      beg_ndx = end_ndx
   
      FOR irec=beg_ndx,nlines_misr-1L DO BEGIN  ; loop over pixels
   
         ;------------------------------------------------------------------
         ; Do this if we found a new orbit or there's only 1 record.
         ;------------------------------------------------------------------
   
         IF ((misr_sort_orbs[irec] NE prev_orbit) OR $
             ((misr_sort_orbs[nlines_misr-1L] EQ prev_orbit) AND $
             rec_done)) THEN BEGIN
   
            date1 = misr_sort_dats[irec]
            time1 = misr_sort_tims[irec]
   
            ;---------------------------------------------------------------
            ; If it's not the beginning of the list, process all the blocks
            ; for the orbit just completed.
            ;---------------------------------------------------------------
   
            IF (prev_orbit NE 0L) THEN BEGIN
   
               ;------------------------------------------------------------
               ; Get the unique block numbers for the previous orbit and
               ; first and last blocks.
               ;------------------------------------------------------------

               blk_list = blk_list[0:iblk-1]
               blk_list = blk_list[UNIQ(blk_list, SORT(blk_list))]
               nblk = N_ELEMENTS(blk_list)
               min_blk = blk_list[0]
               max_blk = blk_list[nblk-1]

               ;------------------------------------------------------------
               ; Break blocks for orbit into manageable chunks and write
               ; orbit/block group data to the MISR orbit process file.
               ;------------------------------------------------------------

               BreakBlkListForOrbit, blk_list, nblk, blkbeglist, $
                                     blkendlist, num_group, retval

               FOR ibreak=0,num_group-1 DO $
                  PRINTF, unit1, FORMAT='(I5,1X,2I5,A14,A13)', $
                          prev_orbit, blkbeglist[ibreak], $
                          blkendlist[ibreak], prev_date, prev_time

               blkbeglist = 0
               blkendlist = 0

               ;------------------------------------------------------------
               ; Add the orbit number to the comma-separated list of orbits
               ; in the MISR orbit order file to use for ordering products
               ; from the Langley DAAC.
               ;------------------------------------------------------------

               WRITEU, unit2, STRTRIM(STRING(prev_orbit),2) + ','

               num_orbit += 1
            ENDIF
   
            ;---------------------------------------------------------------
            ; Set up for the next orbit.
            ;---------------------------------------------------------------
   
            iblk = 0
            blk_list = 0
            blk_list = INTARR(nlines_misr)
            end_ndx = irec
            prev_orbit = misr_sort_orbs[irec]
            prev_date = date1
            prev_time = STRMID(time1, 1, 8)
            BREAK
   
         ENDIF ELSE BEGIN
   
            blk_list[iblk] = misr_sort_blks[irec]
            iblk += 1
            prev_orbit = misr_sort_orbs[irec]
   
            IF (irec EQ nlines_misr-1) THEN BEGIN
               end_ndx = 0L
               rec_done = 1
            ENDIF
         ENDELSE
   
      ENDFOR
   
      IF (irec NE nlines_misr AND rec_done) THEN BREAK
   
   ENDWHILE

   ;------------------------------------------------------------------------
   ; If no MODIS data required, then close files, clean up and set group
   ; file permissions to rw.
   ;------------------------------------------------------------------------
   
   FREE_LUN, unit1

   POINT_LUN, -unit2, end_pos
   POINT_LUN,  unit2, end_pos-1
   WRITEU, unit2, STRING(13b)
   FREE_LUN, unit2

   rtrn_val = ChmodCatchError(out_process_file, '666'O)
   rtrn_val = ChmodCatchError(out_orbit_file,   '666'O)

   ;------------------------------------------------------------------------
   ; Cluster the remaining pixels into fire events (assoc. w/ plumes ?).
   ; Loop over all potential fire events.  Quit if the starting line is
   ; larger than the number of lines in the dataset.
   ;------------------------------------------------------------------------
   
   lat_list = FLTARR(MAX_PLUMES)
   lon_list = FLTARR(MAX_PLUMES)
   orb_list = LONARR(MAX_PLUMES)
   blk_list = INTARR(MAX_PLUMES)
   lin_list = INTARR(MAX_PLUMES)
   smp_list = INTARR(MAX_PLUMES)
   pth_list = INTARR(MAX_PLUMES)
   dat_list = STRARR(MAX_PLUMES)
   tim_list = STRARR(MAX_PLUMES)
   
   fire_sort_list = STRARR(nlines_misr)
   norbits  = 0
   orbitnum = 0L
   ndxbeg   = 0L
   ifirepix = 0L
   
   WHILE(1) DO BEGIN
   
      IF (ndxbeg GE nlines_misr-1) THEN BREAK
      
      ;---------------------------------------------------------------------
      ; Loop over the remaining fire pixels in the input dataset. We
      ; actually break out rather soon each time through.
      ;---------------------------------------------------------------------
      
      numtok = 0  ; The number of fire pixels in the current orbit.
      iline = 0L
      
      FOR iline=ndxbeg,nlines_misr-1 DO BEGIN
      
         ;------------------------------------------------------------------
         ; Determine if this line of data belongs to the same orbit as
         ; previous line. If so, break out to process the previous orbit.
         ; Otherwise, add the data to the list for this orbit.
         ;------------------------------------------------------------------
         
         IF (misr_sort_orbs[iline] NE orbitnum) THEN BEGIN
            ndxbeg = iline
            orbitnum = LONG(misr_sort_orbs[iline])
            norbits += 1
            BREAK
         ENDIF ELSE BEGIN
            lat_list[numtok] = misr_sort_lats[iline]
            lon_list[numtok] = misr_sort_lons[iline]
            orb_list[numtok] = misr_sort_orbs[iline]
            blk_list[numtok] = misr_sort_blks[iline]
            lin_list[numtok] = misr_sort_lins[iline]
            smp_list[numtok] = misr_sort_smps[iline]
            pth_list[numtok] = misr_sort_pths[iline]
            dat_list[numtok] = misr_sort_dats[iline]
            tim_list[numtok] = misr_sort_tims[iline]
            numtok += 1
            IF (iline EQ nlines_misr-1) THEN BEGIN
               ndxbeg = iline
               BREAK
            ENDIF
         ENDELSE
         
      ENDFOR
      
      IF (iline EQ 0) THEN CONTINUE
      
      ;---------------------------------------------------------------------
      ; We have finished collecting data for this date/orbit, so find
      ; clusters of fire pixels and save them.
      ;---------------------------------------------------------------------
      
      fire_list = STRARR(numtok)
      
      ClusterFirePix, lat_list[0:numtok-1], lon_list[0:numtok-1], $
                      blk_list[0:numtok-1], lin_list[0:numtok-1], $
                      smp_list[0:numtok-1], orb_list[0], pth_list[0], $
                      dat_list[0], tim_list[0], numtok, fire_list, numfire
         
      fire_sort_list[nlines_fire:nlines_fire+numfire-1] = $
         fire_list[0:numfire-1]
      nlines_fire += numfire
      fire_list = 0
      
   ENDWHILE
   
   fire_sort_list = fire_sort_list[0:nlines_fire-1]
   
   misr_sort_lats = 0
   misr_sort_lons = 0
   misr_sort_orbs = 0
   misr_sort_blks = 0
   misr_sort_lins = 0
   misr_sort_smps = 0
   misr_sort_pths = 0
   misr_sort_dats = 0
   misr_sort_tims = 0
   
   lat_list = 0
   lon_list = 0
   orb_list = 0
   blk_list = 0
   lin_list = 0
   smp_list = 0
   pth_list = 0
   dat_list = 0
   tim_list = 0
   
   ;------------------------------------------------------------------------
   ; Do another sort on orbit, block, sample.
   ;------------------------------------------------------------------------
   
   sort_order = SORT(fire_sort_list)
   
   final_sort_list = fire_sort_list[sort_order[*]]
   
   ;------------------------------------------------------------------------
   ; Print a report in a message box.
   ;------------------------------------------------------------------------
   
   m_or_m = 'Use ModVolc fire pixels'
   IF (UseModisFirePix EQ 1) THEN m_or_m = 'Use MODIS fire pixels'
   
   s1  = STRING(FORMAT='(A,A,A)', $
      'Fire Pixel Statistics Based on ModVolc Data for Project "', $
      PlumeProjectName, '"')
   s2  = STRING(FORMAT='(A)', 'Generated ' + SYSTIME() + ' by ' + $
      !KON.Misc.UserName)
   s3  =   ' '
   s4  = STRING(FORMAT='(A)',        'User Input Parameters:')
   s5  = STRING(FORMAT='(I12,A)', $
      !SAV.Util.FirstValidBlock, ' = First valid block')
   s6  = STRING(FORMAT='(I12,A)', $
      !SAV.Util.LastValidBlock, ' = Last valid block')
   s7  = STRING(FORMAT='(2x,A10,A)', $
      !SAV.Util.BegDate, ' = Begin date to process')
   s8  = STRING(FORMAT='(2x,A10,A)', $
      !SAV.Util.EndDate, ' = End date to process')
   s9  = STRING(FORMAT='(I12,A)', $
      !SAV.Util.NumFirePixOrbit, ' = Minimum number of fire pixels per orbit')
   s10 = STRING(FORMAT='(I12,A)', $
      !SAV.Util.MaxBlksPerLoad, ' = Maximum # of blocks for MINX to load')
   s11 = STRING(FORMAT='(I12,A)', ~UseModisFirePix, ' = ' + m_or_m)
   s12 =   ' '
   s13 = STRING(FORMAT='(A)', 'Input Data:')
   s14 = STRING(FORMAT='(I12,A)', nlines_total, $
      ' = Number of raw   MODIS fire pixels including Aqua, night-side, ' + $
      'MODIS swath')
   s15 = STRING(FORMAT='(I12,A)', nlines_aqua, $
      ' = Number of Aqua  MODIS fire pixels including night-side, MODIS ' + $
      'swath')
   s16 = STRING(FORMAT='(I12,A)', nlines_terra, $
      ' = Number of Terra MODIS fire pixels including night-side, MODIS ' + $
      'swath')
   s17 = ' '
   s18 = STRING(FORMAT='(A)', 'Rejected Data:')
   s19 = STRING(FORMAT='(I12,A)', nlines_date, $
      ' = Number of Terra MODIS fire pixels rejected: outside requested ' + $
      'date range')
   s21 = STRING(FORMAT='(I12,A)', nlines_badblock, $
      ' = Number of Terra MODIS fire pixels rejected: MISR block outside ' + $
      'requested range  ')
   s22 = STRING(FORMAT='(I12,A)', nlines_swathedge, $
      ' = Number of Terra MODIS fire pixels rejected: on no-data edge of ' + $
      'MISR swath')
   s23 = STRING(FORMAT='(I12,A)', nlines_onepixel, $
      ' = Number of Terra MODIS fire pixels rejected: too few pixels in ' + $
      'orbit')
   s24 = ' '
   s25 = STRING(FORMAT='(A)', 'Accepted Data:')
   s26 = STRING(FORMAT='(I12,A)', nlines_misr, $
      ' = Remaining number of MODIS fire pixels in Terra, day-side, MISR ' + $
      'swath')
   s27 = STRING(FORMAT='(I12,A)', nlines_fire, $
      ' = Estimated maximum number of pixel clusters (fires or smoke ' + $
      'plumes) in project')
   s28 = STRING(FORMAT='(I12,A)', nlines_fire/2, $
      ' = Estimated number of retrievable smoke plumes in project (rough ' + $
      'approximation)')
   s29 = ' '
   s30 = STRING(FORMAT='(I12,A)', num_orbit, $
      ' = Number of MISR orbits to order')
   s31 = ''
   
   IF (UseModisFirePix EQ 1) THEN BEGIN
      s31 = STRING(FORMAT='(I12,A)', num_granules, $
         ' = Number of MODIS MOD14 fire pixel granules to order')
      s32 = ''
      s33 = 'The next step is to find which MISR orbits and blocks to order:'
      s34 = '  1) download the MOD14 granules;'
      s35 = '  2) run MINX Plume Project Utility "Process MODIS MOD14 Fire Granules".'
   ENDIF
   
   IF (UseModisFirePix EQ 0) THEN BEGIN
      mssg = [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,$
              s17,s18,s19,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31]
   ENDIF ELSE BEGIN
      mssg = [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16, $
              s17,s18,s19,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31, $
              s32,s33,s34,s35]
   ENDELSE
   
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   
   ;------------------------------------------------------------------------
   ; Print the same report to a log file and set file permissions to
   ; everyone rw.
   ;------------------------------------------------------------------------
   
   OPENW, unit, out_log_file, /GET_LUN
   FOR ilin=0,N_ELEMENTS(mssg)-1 DO PRINTF, unit, mssg[ilin]
   FREE_LUN, unit
   
   rtrn_val = ChmodCatchError(out_log_file, '666'O)
   
   fire_sort_list = 0
   final_sort_list = 0
   sort_list = 0
   sort_order = 0
   
ENDIF

;---------------------------------------------------------------------------
; If MODIS data was requested, then generate a MODIS granule URL list instead.
;---------------------------------------------------------------------------

IF (UseModisFirePix EQ 1) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Construct the name of the MODIS product files from which to retrieve
   ; the location and fire radiative power values for each orbit. For each
   ; fire pixel, find the MODIS granule that corresponds. Each line in the
   ; file will be a URL identifier. We need to find the date-processed
   ; portion of the file name from the website - it's the only part we 
   ; can't construct from known information.
   ;------------------------------------------------------------------------

   num_granules = 0
   mod14_files = STRARR(nlines_misr)

   FOR irec=0L,nlines_misr-1L DO BEGIN
      date0 = misr_sort_dats[irec]
      ilen = STRLEN(misr_sort_tims[irec])
      time0 = STRMID(misr_sort_tims[irec], 1, ilen-2)

      GetJulianDay, date0, time0, 1, ymd, hms, julian_day

      jul_day = STRTRIM(STRING(FLOOR(julian_day)),2)
      IF (STRLEN(jul_day) LT 3) THEN jul_day = '0' + jul_day
      IF (STRLEN(jul_day) LT 3) THEN jul_day = '0' + jul_day  ;  repeat
      hhmm = hms[0] + hms[1]

      mod14_files[irec] = 'MOD14.A' + ymd[0] + jul_day + '.' + hhmm + '.' + $
                       !SAV.Util.ModisCollection + '.' + '   ' + date0 + $
                       '   ' + time0
   ENDFOR

   ;------------------------------------------------------------------------
   ; Sort the MODIS granule URL identifiers and find the unique ones. Then
   ; set up the common basename for the identifiers.
   ;------------------------------------------------------------------------

   mod_uniq = mod14_files[UNIQ(mod14_files, SORT(mod14_files))]
   num_granules = N_ELEMENTS(mod_uniq)
   mod14_files = 0

   MOD14_base_dir = !SAV.MODISOrderFTPsite

   ;------------------------------------------------------------------------
   ; For each unique MOD14 granule, get the unknown component of the name
   ; (processing date) and create the final full pathnames. URL_obj->Get()
   ; with no output filename and a directory name followed by a slash returns
   ; the contents of passed URL directory. Since granule names are sorted by
   ; date, only call the time-intensive Get() when a new date is encountered.
   ;------------------------------------------------------------------------

   old_date_subdir = ''
   URL_obj = OBJ_NEW('IDLnetUrl')
   
   FOR iuniq=0,num_granules-1 DO BEGIN
      tok1 = STRSPLIT(mod_uniq[iuniq], ' ', /EXTRACT)
      tok2 = STRSPLIT(tok1[1], '-', /EXTRACT)
      date_subdir = tok2[0] + '.' + tok2[1] + '.' + tok2[2] + '/'
      mod14_url_dir = MOD14_base_dir + '/' + date_subdir
      
      ;---------------------------------------------------------------------
      ; URL_obj->Get() with no output filename and a directory name followed
      ; by a slash returns the contents of the passed URL directory. Since
      ; granule names are sorted by date, only call the time-intensive Get()
      ; when a new date is encountered.
      ;---------------------------------------------------------------------
      
      IF (date_subdir NE old_date_subdir) THEN BEGIN
         filenames = STRING(URL_obj->Get(URL=mod14_url_dir, /BUFFER))
         nPos = 0
      ENDIF
      
      ;---------------------------------------------------------------------
      ; The directory listing is a single string, so we need to search in
      ; the string for our partial granule name and separately for the .hdf
      ; extension. Keep incrementing the string position pointer until the
      ; next directory is loaded.
      ;---------------------------------------------------------------------

      old_date_subdir = date_subdir
      mod14_len = 41
      ext = ''
      
      WHILE (nPos GE 0 AND ext NE 'hdf') DO BEGIN
         nPos = STRPOS(filenames, tok1[0], nPos)
         IF nPos EQ -1 THEN BREAK
         mod14_name = STRMID(filenames, nPos, mod14_len)
         nPos += mod14_len
         ext = STRMID(mod14_name, 2, /REVERSE_OFFSET)
      ENDWHILE
      
      ;---------------------------------------------------------------------
      ; Save the completed granule name.
      ;---------------------------------------------------------------------
      
      IF ext EQ 'hdf' THEN mod_uniq[iuniq] = mod14_url_dir + mod14_name
   ENDFOR
   
   ndxs = 0
   filenames = 0
   OBJ_DESTROY, URL_obj
   
   ;------------------------------------------------------------------------
   ; Write the final granule names to file complete with URL information.
   ;------------------------------------------------------------------------
   
   OPENW, unit, out_URL_list_file, /GET_LUN
   FOR iuniq=0,num_granules-1 DO PRINTF, unit, mod_uniq[iuniq]
   mod_uniq = 0
   FREE_LUN, unit
   rtrn_val = ChmodCatchError(out_URL_list_file, '666'O)

ENDIF

;---------------------------------------------------------------------------
; Destroy the message window re. "processing is proceeding".
;---------------------------------------------------------------------------

WIDGET_CONTROL, baseID, /DESTROY

;---------------------------------------------------------------------------
; Allow the user to download MODIS MOD14 files now.
;---------------------------------------------------------------------------

IF (UseModisFirePix EQ 1) THEN BEGIN
   
   sGran = STRTRIM(STRING(num_granules),2)
   mssg = ['Do you want MINX to download the MOD14 granules now?', ' ', $
           'If not, you can download MOD14 granules later using the MINX option', $
           '"Download MODIS MOD14 Fire Granules" on the Plume Utilities menu.', $
           'At that time, choose this file as your list of granules to input:', $
           out_URL_list_file]
   rtrn = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)

   IF (STRUPCASE(rtrn) EQ 'YES') THEN BEGIN

      ;---------------------------------------------------------------------
      ; Let user select the directory where MODIS granules will be downloaded
      ; and stored if required. Create directory if it doesn't exist.
      ;---------------------------------------------------------------------
      
      !SAV.DfltFiles[!KON.FileTyp.TypeMOD14dir].SavePath = DefaultProjDir + $
                     'MOD14_Granules_Modvolc' + !KON.Misc.Slash
         
      GetLastFilename, 0, !KON.FileTyp.TypeMOD14dir, '*', 0, $
                       Mod14GranuleDir, dummy
      IF (Mod14GranuleDir EQ '') THEN RETURN
      
      IF (~ FILE_TEST(Mod14GranuleDir, /DIRECTORY)) THEN BEGIN
         rtrn_val = MakeDirectory(Mod14GranuleDir)
         rtrn_val = ChmodCatchError(Mod14GranuleDir, '777'O)
      ENDIF

      DownloadMOD14Granules, out_URL_list_file, Mod14GranuleDir, status
      IF (status NE 0) THEN RETURN

      mssg = ['Your MOD14 granules have been downloaded to this directory:', $
              Mod14GranuleDir]
      rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      
   ENDIF ELSE BEGIN
      
      mssg = ['You can download the MOD14 granules later using the MINX option', $
              '"Download MODIS MOD14 Fire Granules" in Plume Utilities.', $
              'Choose this file as your list of granules to input:', $
              out_URL_list_file]
      rtrn = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   
   ENDELSE
   
ENDIF

END  ;  ProcessModVolcHotSpots


;***************************************************************************
;***************************************************************************
PRO DownloadMOD14Granules, MOD14_URL_ListFile, MOD14_Granule_Dir, Status
;***************************************************************************
;***************************************************************************
; Procedure reads a file listing MODIS MOD 14 granule URLs for a project
; either downloaded using the ModVolc option above or by the user directly
; from the Reverb website without ModVolc. A sample item in such a file is:
;    http://e4ftl01.cr.usgs.gov/MODIS_Dailies_C/MOLT/MOD14.005/2013.12.31/
;         MOD14.A2013365.2355.005.2014001030532.hdf
; Each file is downloaded into a subdirectory in user's project directory
; with the default name of "MOD14_Granules_<Modvolc or User>/".
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Get the name of the file containing the MOD14 granule URL list from the
; user. Bypass this if the caller passed the file name.
;---------------------------------------------------------------------------

granule_source = 'Modvolc'
MOD14ListFile = MOD14_URL_ListFile

IF (MOD14ListFile EQ '') THEN BEGIN
   Mod14ListDir = !SAV.DfltFiles[!KON.FileTyp.TypeUtilProjDir].SavePath

   GetLastFilename, 0, !KON.FileTyp.TypeMOD14_URLfile, '*.txt', 0, Mod14ListDir, $
                    MOD14ListFile
   IF (MOD14ListFile EQ '') THEN RETURN
   granule_source = 'User'
ENDIF

;---------------------------------------------------------------------------
; Test that the file is of the correct type.
;---------------------------------------------------------------------------

buff = ''
!ERROR_STATE.CODE = 0
ON_IOERROR, bad_format2
bad_format2:
IF (!ERROR_STATE.CODE NE 0) THEN BEGIN
   mssg = ['File ' + MOD14ListFile, $
           'may have the wrong format for a MOD14 granule list file.', $
           'Look for the correct file or fix the contents of this one.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   !ERROR_STATE.CODE = 0
   ON_IOERROR, NULL
   RETURN
ENDIF

OPENR, Unit0, MOD14ListFile, /GET_LUN
READF, Unit0, buff
FREE_LUN, Unit0
IF (STRMID(buff, 0, 14) NE STRMID(!SAV.MODISOrderFTPsite, 0, 14)) THEN BEGIN
   !ERROR_STATE.CODE  = -1
   GOTO, bad_format2
ENDIF
!ERROR_STATE.CODE = 0
ON_IOERROR, NULL

;---------------------------------------------------------------------------
; Count the granules.
;---------------------------------------------------------------------------

numGran = FILE_LINES(MOD14ListFile)

strGran = STRTRIM(STRING(numGran),2)

;---------------------------------------------------------------------------
; Get the name of the subdirectory where the MOD14 granules will be put.
;---------------------------------------------------------------------------

Mod14GranuleDir = MOD14_Granule_Dir

IF (Mod14GranuleDir EQ '') THEN BEGIN
   IF (!SAV.DfltFiles[!KON.FileTyp.TypeMOD14dir].SavePath EQ $
       !SAV.DfltFiles[!KON.FileTyp.TypeUtilProjDir].SavePath OR $
       !SAV.DfltFiles[!KON.FileTyp.TypeMOD14dir].SavePath EQ $
       !KON.Misc.SystemHomeDir ) THEN $
      !SAV.DfltFiles[!KON.FileTyp.TypeMOD14dir].SavePath = $
               !SAV.DfltFiles[!KON.FileTyp.TypeUtilProjDir].SavePath + $
               '_' + granule_source + !KON.Misc.SLASH
   GetLastFilename, 0, !KON.FileTyp.TypeMOD14dir, '*', 0, Mod14GranuleDir, $
                    Mod14GranuleDir
   IF (Mod14GranuleDir EQ '') THEN RETURN
ENDIF

IF (~ FILE_TEST(Mod14GranuleDir, /DIRECTORY)) THEN BEGIN
   rtrn_val = MakeDirectory(Mod14GranuleDir)
   rtrn_val = ChmodCatchError(Mod14GranuleDir, '777'O)
ENDIF

;---------------------------------------------------------------------------
; Show a no-block message window while the download is attempted.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test',UNITS=0,TITLE='Downloading .....', $
                     XOFFSET=!KON.Misc.ScreenX / 2 - 120, $
                     YOFFSET=!KON.Misc.ScreenY / 2 - 50)
mssg = [' ','  MINX is downloading ' + strGran + $
        ' MOD14 granules from the Reverb website.', $
        '  Please be patient ...............................................', ' ']
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=70, YSIZE=4)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK

;---------------------------------------------------------------------------
; Read the MOD14 HDF names, retrieve the files from Reverb and put them in
; the granule directory.
;---------------------------------------------------------------------------

buff = ''
BegMidEnd = 0
OPENR, Unit0, MOD14ListFile, /GET_LUN
URL_obj = OBJ_NEW('IDLnetUrl')

WHILE ~ EOF(Unit0) DO BEGIN
   ReadNextAsciiLine, Unit0, BegMidEnd, buff
   file_part = FILE_BASENAME(buff, FOLD_CASE=1)
   mod14_path = Mod14GranuleDir + file_part
   filename = URL_obj->Get(URL=buff, FILENAME=mod14_path)
   IF (BegMidEnd EQ 3) THEN BREAK
ENDWHILE

OBJ_DESTROY, URL_obj
FREE_LUN, Unit0
WIDGET_CONTROL, baseID, /DESTROY

Status = 0

END  ;  DownloadMOD14Granules


;***************************************************************************
;***************************************************************************
PRO ModisPixelArea, Mline, Msamp, Marea, Status
;***************************************************************************
; Program takes a MODIS line/sample coordinate pair and computes the size of
; the pixel at that location. The size returned is actually the multiplier
; of the size.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Set up constants.
;---------------------------------------------------------------------------

earth_radius = !KON.Misc.earth_radius
orbit_height = !KON.Instr.MISR_altitude
terra_radius = earth_radius + orbit_height
s_param = 0.0014184397
sample_center = 676.5

;---------------------------------------------------------------------------
; Compute the scan angle.
;---------------------------------------------------------------------------

theta = s_param * (Msamp - sample_center)

;---------------------------------------------------------------------------
; Compute the area.
;---------------------------------------------------------------------------

et2_ratio = (earth_radius * earth_radius) / (terra_radius * terra_radius)
sqrt_term = SQRT(et2_ratio - SIN(theta) * SIN(theta))

cross_size = earth_radius * s_param * (COS(theta) / sqrt_term - 1.0)
along_size = terra_radius * s_param * (COS(theta) - sqrt_term)

Marea = cross_size * along_size

Status = 0

END  ;  ModisPixelArea

;***************************************************************************
PRO ModisCrdToMisrCrd, Path, Lat, Lon, Block, Line, Samp, nBlkFail, $
                       nSampFail, Status
;***************************************************************************
; Find MISR path, block, line and sample from the MODIS date and time.
; Points that
; are on the night-side or are not within the MISR swath are automatically
; eliminated.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Get the MISR block, line and sample for this point from lat-lon.
;---------------------------------------------------------------------------

LatLonToBLS, Path, Lat, Lon, Block, Line, Samp

;---------------------------------------------------------------------------
; Filter out blocks not in the desired range or fire pixels on the no-data
; edges of the MISR swath.
;---------------------------------------------------------------------------

IF (Block LT !SAV.Util.FirstValidBlock OR $
    Block GT !SAV.Util.LastValidBlock) THEN BEGIN
   nBlkFail += 1
   RETURN
ENDIF

IF (samp LT !SAV.Util.FirstValidSamp OR $
    samp GT !SAV.Util.LastValidSamp) THEN BEGIN
   nSampFail += 1
   RETURN
ENDIF

Status = 0

END  ;  ModisCrdToMisrCrd

;***************************************************************************
PRO ReadATTR, Filename, Unit, SD_ID, ATTRname, Attribute, Status
;***************************************************************************
; Read a SDS attribute in a MODIS MOD14 file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Find the SDS index to the requested attribute.
;---------------------------------------------------------------------------

ndx = -1
ndx = HDF_SD_ATTRFIND(sd_id, ATTRname)
IF (ndx LT 0) THEN BEGIN
   PRINTF, Unit, Filename + ': could not read attribute "' + ATTRname + '".'
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Read the requested attribute.
;---------------------------------------------------------------------------

HDF_SD_ATTRINFO, sd_id, ndx, DATA=Attribute

Status = 0

END  ;  ReadATTR

;***************************************************************************
PRO ReadSDS, Filename, Unit, SD_ID, SDSname, DataBuffer, NElem, Status
;***************************************************************************
; Read a SDS field in a MODIS MOD14 file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1
DataBuffer = 0

;---------------------------------------------------------------------------
; Find the SDS index to the requested data.
;---------------------------------------------------------------------------

ndx = -1
ndx = HDF_SD_NAMETOINDEX(SD_ID, SDSname)
IF (ndx LT 0) THEN BEGIN
   PRINTF, Unit, Filename + ': could not find HDF index to SDS "' + SDSname + '".'
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Select and read the requested SDS.
;---------------------------------------------------------------------------

sds_id = -1
sds_id = HDF_SD_SELECT(SD_ID, ndx)
IF (sds_id LT 0) THEN BEGIN
   PRINTF, Unit, Filename + ': could not read SDS "' + SDSname + '".'
   RETURN
ENDIF

HDF_SD_GETDATA, sds_id, DataBuffer

;---------------------------------------------------------------------------
; Close the SDS.
;---------------------------------------------------------------------------

HDF_SD_ENDACCESS, sds_id

;---------------------------------------------------------------------------
; Get the size of the SDS.
;---------------------------------------------------------------------------

NElem = N_ELEMENTS(DataBuffer)

Status = 0

END  ;  ReadSDS

;***************************************************************************
PRO ReadMod14FireData, UnitF, Mod14FileName, JulBeg, JulEnd, Orbit, $
                       Path, Date, FirePixList, nTotPixel, nGoodPix, $
                       nErrorFail, nPathFail, nDateFail, nDayFail, $
                       nBlkFail, nSampFail, Status
;***************************************************************************
; Read a MODIS level 2 fire product (MOD14) and retrieve all the pixels that
; indicate fire. For each pixel, also return latitude and longitude, sample
; and line, radiative power in megawatts, 3 channels of brightness
; temperature and confidence level.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Initialize data.
;---------------------------------------------------------------------------

Orbit = ''
Path  = ''
Date  = ''
nlat  = 0

;---------------------------------------------------------------------------
; Create a list of HDF file SDS names for fields to be read.
;---------------------------------------------------------------------------

sds_names = $
  ['fire mask', 'FP_latitude', 'FP_longitude', 'FP_sample', 'FP_line', $
   'FP_power', 'FP_R2', 'FP_T21', 'FP_T31', 'FP_MeanT21', 'FP_MeanT31', $
   'FP_confidence', 'algorithm QA']

;---------------------------------------------------------------------------
; Open the MODIS MOD14 file for SDS read.
;---------------------------------------------------------------------------

sd_id = HDF_StartInterface(Mod14FileName)
IF (sd_id LE 0) THEN RETURN

;---------------------------------------------------------------------------
; Set an error handler.
;---------------------------------------------------------------------------

CATCH, error_status
IF (error_status NE 0) THEN GOTO, error_rtrn

;---------------------------------------------------------------------------
; Get the  product version, orbit number, date and time from the metadata of
; this granule. Skip the file if there are no daytime pixels or fire pixels.
;---------------------------------------------------------------------------

ReadATTR, Mod14FileName, UnitF, sd_id, 'FirePix', num_fire_pix, Retval
IF (Retval LT 0) THEN GOTO, error_rtrn
nTotPixel += num_fire_pix

ReadATTR, Mod14FileName, UnitF, sd_id, 'DayPix', num_day_pix, Retval
IF (Retval LT 0) THEN GOTO, error_rtrn

IF (num_day_pix LE 0 OR num_fire_pix LE 0) THEN GOTO, nodata_rtrn

ReadATTR, Mod14FileName, UnitF, sd_id, 'ProcessVersionNumber', version, Retval
IF (Retval LT 0) THEN GOTO, error_rtrn

MODIScollection = STRMID(version, 0, 1)

ReadATTR, Mod14FileName, UnitF, sd_id, 'CoreMetadata.0', coremetadata, Retval
IF (Retval LT 0) THEN GOTO, error_rtrn

ipos  = STRPOS(coremetadata, 'ORBITNUMBER')
ipos  = STRPOS(coremetadata, 'VALUE', ipos)
ipos  = STRPOS(coremetadata, '=', ipos)
Orbit = STRMID(coremetadata, ipos+2, 5)

ipos = STRPOS(coremetadata, 'EQUATORCROSSINGDATE')
ipos = STRPOS(coremetadata, 'VALUE', ipos)
ipos = STRPOS(coremetadata, '=', ipos)
Date = STRMID(coremetadata, ipos+3, 10)

ipos = STRPOS(coremetadata, 'EQUATORCROSSINGTIME')
ipos = STRPOS(coremetadata, 'VALUE', ipos)
ipos = STRPOS(coremetadata, '=', ipos)
Time = STRMID(coremetadata, ipos+3, 11)

coremetadata = 0

;---------------------------------------------------------------------------
; Compute path from orbit.
;---------------------------------------------------------------------------

orbitnum = LONG(Orbit)
pathnum  = PathFromOrbit(orbitnum)
Path  = STRTRIM(STRING(pathnum),2)
Orbit = STRTRIM(STRING(orbitnum),2)

;---------------------------------------------------------------------------
; Read the SDS values for the latitude field.
;---------------------------------------------------------------------------

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[1], data_lats, nlat, statval
IF (statval LT 0) THEN GOTO, error_rtrn

;---------------------------------------------------------------------------
; FILTER out the ** PATH ** if it is not within the user-specified range.
;---------------------------------------------------------------------------

Path = PathFromOrbit(orbitnum)

IF (((!SAV.Util.FirstValidPath LE !SAV.Util.LastValidPath) AND $
     (Path LT !SAV.Util.FirstValidPath  OR $
      Path GT !SAV.Util.LastValidPath)) OR $
    ((!SAV.Util.FirstValidPath GT !SAV.Util.LastValidPath) AND $
     (Path LT !SAV.Util.FirstValidPath AND $
      Path GT !SAV.Util.LastValidPath))) THEN BEGIN

   nPathFail += nlat
   GOTO, nodata_rtrn
ENDIF

;---------------------------------------------------------------------------
; FILTER out the ** DATE ** if it is not within the user-specified range.
; The format of Date, Time parameters is: 'YYYY-MM-DD, hh:mm:ss.ss'. Adjust
; the dayside equator-crossing time for latitude for each point. The constant
; in JulianTime calculation is the fraction of a day represented by one orbit
; (16.0 / 233.0) divided by 360 degrees.
;---------------------------------------------------------------------------

GetJulianDay, Date, Time, 0, ymd, hms, JulianTime

JulianTime -= data_lats * 0.00019074869D
ok_ndxs = WHERE(JulianTime GE JulBeg AND JulianTime LE JulEnd, numndxs)
IF (numndxs EQ 0) THEN GOTO, nodata_rtrn

;---------------------------------------------------------------------------
; Read the SDS values for all remaining desired fields.
;---------------------------------------------------------------------------

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[2], data_lons, nlon, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[3], data_samp, nsamp, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[4], data_line, nline, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[5], data_power, npower, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[6], data_btR2, nbtR2, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[7], data_btT21, nbtT21, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[8], data_btT31, nbtT31, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[9], data_btmT21, nbtmT21, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[10], data_btmT31, nbtmT31, statval
IF (statval LT 0) THEN GOTO, error_rtrn

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[11], data_conf, nconf, statval
IF (statval LT 0) THEN GOTO, error_rtrn

;---------------------------------------------------------------------------
; This 32-bit unsigned integer (data_QA) is bit-encoded and contains a flag
; for day/night algorithm. It's the best information available for filtering
; out night pixels. The reflectance SDS (data_btR2) might also be used.
;---------------------------------------------------------------------------

ReadSDS, Mod14FileName, UnitF, sd_id, sds_names[12], data_QA, nQA, statval
IF (statval LT 0) THEN GOTO, error_rtrn

;---------------------------------------------------------------------------
; Close the MODIS MOD14 file.
;---------------------------------------------------------------------------

CATCH, /CANCEL
HDF_SD_END, sd_id

;---------------------------------------------------------------------------
; Make sure all the SDSs are the same size.
;---------------------------------------------------------------------------

IF ((nlat  NE nlon)   OR (nlon   NE nsamp) OR (nsamp NE nline) OR $
    (nline NE npower) OR (npower NE nconf)) THEN BEGIN

   PRINTF, UnitF, Mod14FileName + ': not all SDSs have the same size.'
   nErrorFail += nlat
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Create an array of structures to contain the MODIS fire pixel data that
; pass the filtering tests.
;---------------------------------------------------------------------------

FOR ipix=0,nlat-1 DO BEGIN

   ndxs = WHERE(ok_ndxs EQ ipix, numpix)
   IF (numpix EQ 0) THEN BEGIN
      nDateFail += 1
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Get the indices into the data_QA parameter.
   ;------------------------------------------------------------------------

   samp = data_samp[ipix]
   line = data_line[ipix]

   ;------------------------------------------------------------------------
   ; FILTER out fire pixels if the ** DAY/NIGHT ** flag in the QA parameter
   ; is set for the night algorithm. This flag is bit 4 (0-based). A value
   ; of 1 is day, so perform a bitwise AND with 16.
   ;------------------------------------------------------------------------

   IF ((data_QA[samp,line] AND 16) EQ 0) THEN BEGIN
      nDayFail += 1
      CONTINUE
   ENDIF

   ;------------------------------------------------------------------------
   ; Compute the area of the MODIS pixel for collection 4 data. This
   ; calculation has already been done in the collection 5 data. The area
   ; of a pixel in the center of swath is 1 km**2. Fire power is reported
   ; as watt/sq m, which is equivalent to Mwatts/sq km. So we can just
   ; multiply the power in watts by the area in sq km to get Mwatts.
   ;------------------------------------------------------------------------

   area = 1.0
   IF (MODIScollection EQ '4') THEN $
      ModisPixelArea, data_line[ipix], data_samp[ipix], area, statval

   mod_power = data_power[ipix] * area

   ;------------------------------------------------------------------------
   ; Convert the MOD14 lat and lon to MISR orbit, block, line and sample.
   ; In the process, FILTER out pixels that don't satisfy the ** BLOCK **
   ; and ** SAMPLE ** (edge of swath) requirements.
   ;------------------------------------------------------------------------

   ModisCrdToMisrCrd, Path, data_lats[ipix], data_lons[ipix], block, misrline, $
                      misrsamp, nBlkFail, nSampFail, statval

   IF (statval NE 0) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Fill the entries in the fire pixel structure. Note that we must adjust
   ; the power/area by the pixel area, which changes with distance from the
   ; center of the swath (only for collection 4).
   ;------------------------------------------------------------------------

   FirePixList[nGoodPix].latitude        = data_lats[ipix]
   FirePixList[nGoodPix].longitude       = data_lons[ipix]
   FirePixList[nGoodPix].date            = Date
   FirePixList[nGoodPix].orbit           = orbitnum
   FirePixList[nGoodPix].block           = block
   FirePixList[nGoodPix].MISRsamp        = ROUND(misrsamp)
   FirePixList[nGoodPix].MISRline        = ROUND(misrline)
   FirePixList[nGoodPix].mean_power      = mod_power
   FirePixList[nGoodPix].reflectance_R2  = data_btR2[ipix]
   FirePixList[nGoodPix].brght_temp_T21  = data_btT21[ipix]
   FirePixList[nGoodPix].brght_temp_T31  = data_btT31[ipix]
   FirePixList[nGoodPix].brght_temp_mT21 = data_btmT21[ipix]
   FirePixList[nGoodPix].brght_temp_mT31 = data_btmT31[ipix]
   FirePixList[nGoodPix].fp_confidence   = data_conf[ipix]

   nGoodPix += 1

ENDFOR

data_lats = 0
data_lons = 0
data_samp = 0
data_line = 0
data_power = 0
data_btR2 = 0
data_btT21 = 0
data_btT31 = 0
data_btmT21 = 0
data_btmT31 = 0
data_conf = 0

Status = 0
RETURN

error_rtrn:      ; Here on error
nErrorFail += num_fire_pix

nodata_rtrn:     ; Here if no pertinent data
CATCH, /CANCEL
IF (sd_id GE 0) THEN HDF_SD_END, sd_id

data_lats = 0
data_lons = 0
data_samp = 0
data_line = 0
data_power = 0
data_btR2 = 0
data_btT21 = 0
data_btT31 = 0
data_btmT21 = 0
data_btmT31 = 0
data_conf = 0

END  ;  ReadMod14FireData

;***************************************************************************
PRO WriteMINXFireData, Unit1, Unit2, FirePixDir, PixFileTemplate, $
                       FirePixList, GoodFirepix, GoodBlocks, GoodGroups, $
                       GoodOrbits, nPixCnfFail, nPixPowFail, Retval
;***************************************************************************
; Process the fire pixels that passed the first filtering stage. Collect the
; pixels into orbits and block groups (chunks of blocks small enough to be
; loaded into MINX - set by user). Filter out groups and orbits that have
; too few pixels and orbits with too small a cumulative fire power. Write
; the fire pixel files. The purpose of filtering is to eliminate the least
; significant data to reduce the time spent loading large numbers of block-
; groups that have little value.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

MAX_ORBITS = 10000L
MAX_GROUPS = 50000L
MAX_BLOCKS = 100000L

Retval = -1

;---------------------------------------------------------------------------
; Create a comprehensive, unique list of [orbit, block, samp, line] values
; coded as a string and sort the fire pixel list using these strings.
;---------------------------------------------------------------------------

orbit_list = FirePixList[*].orbit
block_list = FirePixList[*].block
samp_list  = FirePixList[*].misrsamp
line_list  = FirePixList[*].misrline

nPixIn = N_ELEMENTS(FirePixList)
orb_blk_smp_lin_str = STRARR(nPixIn)

FOR ipix=0L,nPixIn-1 DO BEGIN
   orb_str = STRTRIM(STRING(orbit_list[ipix]),2)
   IF (STRLEN(orb_str)  LT 5) THEN orb_str  = '0' + orb_str
   blk_str = STRTRIM(STRING(block_list[ipix]),2)
   IF (STRLEN(blk_str)  LT 2) THEN blk_str  = '0' + blk_str
   IF (STRLEN(blk_str)  LT 3) THEN blk_str  = '0' + blk_str
   samp_str = STRTRIM(STRING(samp_list[ipix]),2)
   IF (STRLEN(samp_str) LT 2) THEN samp_str = '0' + samp_str
   IF (STRLEN(samp_str) LT 3) THEN samp_str = '0' + samp_str
   IF (STRLEN(samp_str) LT 4) THEN samp_str = '0' + samp_str
   line_str = STRTRIM(STRING(line_list[ipix]),2)
   IF (STRLEN(line_str) LT 2) THEN line_str = '0' + line_str
   IF (STRLEN(line_str) LT 3) THEN line_str = '0' + line_str

   orb_blk_smp_lin_str[ipix] = orb_str + blk_str + line_str + samp_str
ENDFOR

FirePixList = FirePixList[SORT(orb_blk_smp_lin_str)]

orbit_list = 0
block_list = 0
samp_list = 0
line_list = 0
orb_blk_smp_lin_str = 0

;---------------------------------------------------------------------------
; Create 2 arrays of indices that point to the first and last fire pixel
; locations for each orbit consisting of all fire pixels in the original
; list. These will be used to write the fire pixels to file.
;---------------------------------------------------------------------------

orbit_list_temp = [0L, FirePixList[*].orbit, 0L]

ndx_orb_prev = WHERE(FirePixList.orbit NE orbit_list_temp[0:nPixIn-1], $
                     numndx_orb_prev)
ndx_orb_next = WHERE(FirePixList.orbit NE orbit_list_temp[2:nPixIn+1], $
                     numndx_orb_next)
orbit_list_temp = 0

;---------------------------------------------------------------------------
; Loop over all the orbits in the original list.
;---------------------------------------------------------------------------

FOR iorb=0L,numndx_orb_prev-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Isolate the orbit number and create a subset of all its fire pixels.
   ; Also get path number and date as strings.
   ;------------------------------------------------------------------------

   orbitnum  = FirePixList[ndx_orb_prev[iorb]].orbit
   orbit_str = STRTRIM(STRING(orbitnum),2)
   pathnum   = PathFromOrbit(orbitnum)
   path_str  = STRTRIM(STRING(pathnum), 2)
   date      = FirePixList[ndx_orb_prev[iorb]].date

   ;------------------------------------------------------------------------
   ; Construct the name of the file to contain fire pixel data for the orbit
   ; and open it. If the file exists, add to the end. These files contain
   ; all the fire pixels available for their orbits, even if filtered out
   ; below by user choices.
   ;------------------------------------------------------------------------

   toks = STRSPLIT(PixFileTemplate, '__', /EXTRACT, /REGEX)

   fire_pixel_file = FirePixDir + toks[0] + '_' + '0' + orbit_str + '_' + toks[1]
   info_struct = FILE_INFO(fire_pixel_file)
   new_file = 1

   IF (info_struct.EXISTS) THEN BEGIN
      OPENW, Unit3, fire_pixel_file, /GET_LUN, /APPEND
      new_file = 0
   ENDIF ELSE BEGIN
      OPENW, Unit3, fire_pixel_file, /GET_LUN
   ENDELSE

   ;------------------------------------------------------------------------
   ; Write the MOD14 data for this orbit to its MISR fire pixel file and
   ; close the file when done. Even orbits that do not pass the filtering
   ; tests below will have a fire pixel file written.
   ;------------------------------------------------------------------------

   IF (new_file) THEN BEGIN
      PRINTF, Unit3, 'Fire pixels from MODIS granules on 275m MISR SOM ' + $
                     'grid for project : ' + !SAV.Util.ProjectName
      PRINTF, Unit3, orbit_str + ' / ' + path_str + ' / ' + date + $
                     '  : orbitnum / pathnum / date'
      PRINTF, Unit3, ' Longitude  Latitude Blk Samp Line  Power ' + $
                     'ReflR2 BTmpT21 BTmpT31 BBTmpT21 BBTmpT31 Conf'
      PRINTF, Unit3, '  degrees    degrees      0-based   MWatt ' + $
                     'reflec fire(k) fire(k) bkgnd(k) bkgnd(k)   %'
   ENDIF

   FOR ipix=ndx_orb_prev[iorb],ndx_orb_next[iorb] DO BEGIN
      PRINTF, Unit3, FORMAT='(2F10.5,I4,I5,I5,F7.1,F7.3,F7.1,F8.1,2F9.1,I6)', $
                     FirePixList[ipix].longitude, $
                     FirePixList[ipix].latitude, $
                     FirePixList[ipix].block, $
                     FirePixList[ipix].misrsamp, $
                     FirePixList[ipix].misrline, $
                     FirePixList[ipix].mean_power, $
                     FirePixList[ipix].reflectance_R2, $
                     FirePixList[ipix].brght_temp_T21, $
                     FirePixList[ipix].brght_temp_T31, $
                     FirePixList[ipix].brght_temp_mT21, $
                     FirePixList[ipix].brght_temp_mT31, $
                     FirePixList[ipix].fp_confidence
   ENDFOR

   FREE_LUN, Unit3
   rtrn_val = ChmodCatchError(fire_pixel_file, '666'O)

ENDFOR

;---------------------------------------------------------------------------
; Filter out fire pixels if they have lower CONFIDENCE than user's threshold.
; Store them in a new fire pixel array.
;---------------------------------------------------------------------------

ndxs_succ = WHERE(FirePixList[*].fp_confidence GE !SAV.Util.ConfThresh, $
                  numndxs_succ, NCOMPLEMENT=numndxs_fail)
nPixCnfFail += numndxs_fail

IF (numndxs_succ GT 0) THEN BEGIN
   GoodFirepix = FirePixList[ndxs_succ]
ENDIF ELSE BEGIN
   ndxs_succ = 0
   RETURN
ENDELSE

ndxs_succ = 0

;---------------------------------------------------------------------------
; From the remaining list, filter out fire pixels if they have a lower
; PIXEL POWER than user's threshold.
;---------------------------------------------------------------------------

ndxs_succ = WHERE(GoodFirepix[*].mean_power GE !SAV.Util.MinPixPower, $
                  numndxs_succ, NCOMPLEMENT=numndxs_fail)
nPixPowFail += numndxs_fail

IF (numndxs_succ GT 0) THEN BEGIN
   GoodFirepix = GoodFirepix[ndxs_succ]
ENDIF ELSE BEGIN
   ndxs_succ = 0
   RETURN
ENDELSE

ndxs_succ = 0

;---------------------------------------------------------------------------
; Create 2 arrays of indices that point to the first and last fire pixel
; locations for each orbit consisting of only fire pixels in a new list that
; has filtered out those fire pixels that don't pass the threshold tests. Do
; this to efficiently compile the list and to count sorted orbits.
;---------------------------------------------------------------------------

orbit_list = [0L, GoodFirepix[*].orbit, 0L]
num_pixels = N_ELEMENTS(GoodFirepix)

ndx_orb_prev = WHERE(GoodFirepix.orbit NE orbit_list[0:num_pixels-1], $
                         numndx_orb_prev)
ndx_orb_next = WHERE(GoodFirepix.orbit NE orbit_list[2:num_pixels+1], $
                         numndx_orb_next)
orbit_list = 0

num_orbits = numndx_orb_prev
IF (num_orbits GE MAX_ORBITS) THEN BEGIN
   mssg = ['The maximum number of successful MISR orbits allowed in ' + $
           'one run is ' + STRTRIM(STRING(MAX_ORBITS),2), $
           'Reduce the size of this run and try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   ndx_orb_prev = 0
   ndx_orb_next = 0
   RETURN
ENDIF

orbit_list = GoodFirepix[ndx_orb_prev].orbit

;---------------------------------------------------------------------------
; Create a return array for the block groups.
;---------------------------------------------------------------------------

group_struct = {orbit : 0L, blk_beg : 0, blk_end : 0, num_pix : 0L, $
                tot_power : 0}
GoodGroups = REPLICATE(group_struct, MAX_GROUPS)
nGroups = 0L
nFirepix = 0L

;---------------------------------------------------------------------------
; Loop over all the orbits.
;---------------------------------------------------------------------------

FOR iorb=0L,num_orbits-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Isolate the orbit number and create a subset of all its fire pixels.
   ; Also get path number and date as strings.
   ;------------------------------------------------------------------------

   orbitnum = orbit_list[iorb]
   firepix_orbit = GoodFirepix[ndx_orb_prev[iorb]:ndx_orb_next[iorb]]

   orbit_str = STRTRIM(STRING(orbitnum),2)
   pathnum   = PathFromOrbit(orbitnum)
   path_str  = STRTRIM(STRING(pathnum), 2)
   date      = GoodFirepix[ndx_orb_prev[iorb]].date

   ;------------------------------------------------------------------------
   ; Collect all the unique block numbers for current orbit in sorted order.
   ;------------------------------------------------------------------------

   block_list = firepix_orbit[*].block
   block_list = block_list[UNIQ(block_list, SORT(block_list))]
   nblk = N_ELEMENTS(block_list)

   ;------------------------------------------------------------------------
   ; Loop over the blocks and find all fire pixels remaining in the list.
   ; Compute their cumulative fire power. If the BLOCK POWER is less than the
   ; threshold, filter out all the block's fire pixels. Otherwise, keep all.
   ; If there are no qualifying fire pixels in any block, skip to next orbit.
   ;------------------------------------------------------------------------

   num_succ_block = 0

   FOR iblk=0L,nblk-1 DO BEGIN
      ndxs1 = WHERE(firepix_orbit[*].block EQ block_list[iblk], numndxs1, $
                    COMPLEMENT=ndxs2, NCOMPLEMENT=numndxs2)

      tot_power = (numndxs1 EQ 0) ? 0.0 : TOTAL(firepix_orbit[ndxs1].mean_power)

      IF (tot_power LT !SAV.Util.MinBlkPower) THEN BEGIN
         IF (numndxs2 EQ 0) THEN CONTINUE
         firepix_orbit = firepix_orbit[ndxs2]
      ENDIF ELSE BEGIN
         num_succ_block += 1
      ENDELSE
   ENDFOR

   IF (num_succ_block EQ 0) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Collect all the remaining unique block numbers for the current orbit
   ; in sorted order and break the blocks for the orbit into manageable
   ; chunks or block groups.
   ;------------------------------------------------------------------------

   block_list = firepix_orbit[*].block
   block_list = block_list[UNIQ(block_list, SORT(block_list))]
   nblk = N_ELEMENTS(block_list)

   BreakBlkListForOrbit, block_list, nblk, blkbeglist, blkendlist, $
                         num_group, status

   ;------------------------------------------------------------------------
   ; Loop over the block-groups and find all its fire pixels from remaining
   ; list. Compute the cumulative fire power. If GROUP POWER is less than
   ; the threshold, filter out all group's fire pixels. Otherwise, keep all.
   ; If there are no qualifying fire pixels in any group, skip to next orbit.
   ;------------------------------------------------------------------------

   num_succ_group = 0

   FOR igrp=0L,num_group-1 DO BEGIN
      add_blk = 1
      noadd_blk = 1
      IF (igrp GT 0L) THEN noadd_blk = (blkbeglist[igrp] EQ blkendlist[igrp-1]) ? 1 : 0
      IF ((block_list[0] EQ 1 AND blkbeglist[igrp] EQ 1) OR noadd_blk) THEN add_blk = 0
      sub_blk = 1
      IF (block_list[nblk-1] EQ 180 AND blkendlist[igrp] EQ 180) THEN sub_blk = 0

      ndxs1 = WHERE(firepix_orbit[*].orbit EQ orbitnum AND $
                    firepix_orbit[*].block GE blkbeglist[igrp]+add_blk AND $
                    firepix_orbit[*].block LE blkendlist[igrp]-sub_blk, $
                    numndxs1, COMPLEMENT=ndxs2, NCOMPLEMENT=numndxs2)

      tot_power = (numndxs1 EQ 0) ? 0.0 : TOTAL(firepix_orbit[ndxs1].mean_power)

      IF (tot_power LT !SAV.Util.MinGrpPower) THEN BEGIN
         IF (numndxs2 EQ 0) THEN CONTINUE
         firepix_orbit = firepix_orbit[ndxs2]
      ENDIF ELSE BEGIN
         num_succ_group += 1
      ENDELSE
   ENDFOR

   IF (num_succ_group EQ 0) THEN CONTINUE

   ;------------------------------------------------------------------------
   ; Collect all the remaining unique block numbers for the current orbit
   ; in sorted order and again break the blocks for the orbit into manageable
   ; chunks or block groups if the number of available blocks has changed.
   ;------------------------------------------------------------------------

   block_list = firepix_orbit[*].block
   block_list = block_list[UNIQ(block_list, SORT(block_list))]
   nblk = N_ELEMENTS(block_list)

   BreakBlkListForOrbit, block_list, nblk, blkbeglist, blkendlist, $
                         num_group, status

   ;------------------------------------------------------------------------
   ; Loop over the remaining block-groups and find count its fire pixels in
   ; the remaining list. If the GROUP NUMBER is less than threshold, filter
   ; out all the group's fire pixels. Otherwise, keep them all.
   ;------------------------------------------------------------------------

   FOR igrp=0L,num_group-1 DO BEGIN
      add_blk = 1
      noadd_blk = 1
      IF (igrp GT 0L) THEN noadd_blk = (blkbeglist[igrp] EQ blkendlist[igrp-1]) ? 1 : 0
      IF ((block_list[0] EQ 1 AND blkbeglist[igrp] EQ 1) OR noadd_blk) THEN add_blk = 0
      sub_blk = 1
      IF (block_list[nblk-1] EQ 180 AND blkendlist[igrp] EQ 180) THEN sub_blk = 0

      ndxs1 = WHERE(firepix_orbit[*].orbit EQ orbitnum AND $
                    firepix_orbit[*].block GE blkbeglist[igrp]+add_blk AND $
                    firepix_orbit[*].block LE blkendlist[igrp]-sub_blk, $
                    numndxs1, COMPLEMENT=ndxs2, NCOMPLEMENT=numndxs2)

      IF (numndxs1 LT !SAV.Util.NumFirePixGroup) THEN BEGIN
         IF (numndxs2 EQ 0) THEN CONTINUE
         firepix_orbit = firepix_orbit[ndxs2]
      ENDIF ELSE BEGIN
         GoodFirepix[nFirepix:nFirepix+numndxs1-1] = firepix_orbit[ndxs1]
         nFirepix += numndxs1
      ENDELSE
   ENDFOR

ENDFOR

;---------------------------------------------------------------------------
; Intermediate clean up.
;---------------------------------------------------------------------------

GoodFirepix = GoodFirepix[0:nFirepix-1]

ndx_orb_prev = 0
ndx_orb_next = 0
orbit_list = 0
firepix_orbit = 0
block_list = 0
blkbeglist = 0
blkendlist = 0
ndxs1 = 0
ndxs2 = 0

;---------------------------------------------------------------------------
; Using the list of successful fire pixels, create a comprehensive, unique
; list of paired [orbit, block] values coded as a string for sorting.
;---------------------------------------------------------------------------

orbit_list = GoodFirepix[*].orbit
block_list = GoodFirepix[*].block
GoodFirepix = 0

orb_blk_str = STRARR(nFirepix)

FOR ipix=0L,nFirepix-1 DO BEGIN
   blk_str = STRTRIM(STRING(block_list[ipix]),2)
   IF (STRLEN(blk_str) LT 2) THEN blk_str = '0' + blk_str
   IF (STRLEN(blk_str) LT 3) THEN blk_str = '0' + blk_str
   orb_str = STRTRIM(STRING(orbit_list[ipix]),2)
   IF (STRLEN(orb_str) LT 5) THEN orb_str = '0' + blk_str
   orb_blk_str[ipix] = orb_str + blk_str
ENDFOR
orbit_list = 0
block_list = 0

orb_blk_str = orb_blk_str[UNIQ(orb_blk_str, SORT(orb_blk_str))]

norb = N_ELEMENTS(orb_blk_str)
orbit_list = LONG(STRMID(orb_blk_str[*], 0, 5))
block_list =  FIX(STRMID(orb_blk_str[*], 5, 3))
orb_blk_str = 0

;---------------------------------------------------------------------------
; Create the return array in which to accumulate all successful fire pixels.
; This may differ from the earlier count, because low power pixels previously
; excluded will be counted here if they happen to fall in block groups that
; are successful for other reasons.
;---------------------------------------------------------------------------

GoodFirepix = FirePixList

;---------------------------------------------------------------------------
; Loop over the [orbit, block] pairs and, for each, find all the fire pixels
; in the original list of fire pixels that belong to it.
;---------------------------------------------------------------------------

ibegpix = 0L
iendpix = nPixIn-1
nFirepix = 0L

FOR iorb=0L,norb-1 DO BEGIN
   FOR ifirepix=ibegpix,iendpix DO BEGIN

      IF (FirePixList[ifirepix].orbit GT orbit_list[iorb]) THEN BEGIN
         ibegpix = ifirepix
         BREAK
      ENDIF
      IF (FirePixList[ifirepix].orbit LT orbit_list[iorb]) THEN CONTINUE

      IF (FirePixList[ifirepix].block GT block_list[iorb]) THEN BEGIN
         ibegpix = ifirepix
         BREAK
      ENDIF
      IF (FirePixList[ifirepix].block LT block_list[iorb]) THEN CONTINUE

      GoodFirepix[nFirepix] = FirePixList[ifirepix]
      nFirepix += 1

   ENDFOR
ENDFOR

GoodFirepix = GoodFirepix[0L:nFirepix-1]

;---------------------------------------------------------------------------
; Get a comprehensive and unique list of remaining orbits.
;---------------------------------------------------------------------------

new_orbit_list = orbit_list[UNIQ(orbit_list, SORT(orbit_list))]
nOrbits = N_ELEMENTS(new_orbit_list)

;---------------------------------------------------------------------------
; Create arrays of structs to contain successful orbits and blocks to return.
;---------------------------------------------------------------------------

orbit_struct = {orbit : 0L, num_pix : 0L, tot_power : 0}
GoodOrbits = REPLICATE(orbit_struct, nOrbits)

block_struct = {orbit : 0L, block : 0, num_pix : 0L, tot_power : 0}
GoodBlocks = REPLICATE(block_struct, MAX_BLOCKS)

;---------------------------------------------------------------------------
; Loop over all remaining orbits one last time and accumulate data to return.
;---------------------------------------------------------------------------

nGroups = 0L
nBlocks = 0L

FOR iorb=0L,nOrbits-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; On orbits. And add the orbit number to the comma-separated list in the
   ; "Order" file.
   ;------------------------------------------------------------------------

   orbitnum = new_orbit_list[iorb]
   ndxs1 = WHERE(GoodFirepix[*].orbit EQ orbitnum, numndxs1)
   firepix_orbit = GoodFirepix[ndxs1]

   GoodOrbits[iorb].orbit = orbitnum
   GoodOrbits[iorb].num_pix = numndxs1
   GoodOrbits[iorb].tot_power = ROUND(TOTAL(firepix_orbit[*].mean_power))

   WRITEU, Unit2, STRTRIM(STRING(orbitnum),2) + ','

   ;------------------------------------------------------------------------
   ; On block groups.
   ;------------------------------------------------------------------------

   block_list = firepix_orbit[*].block
   block_list = block_list[UNIQ(block_list, SORT(block_list))]
   nblk = N_ELEMENTS(block_list)

   BreakBlkListForOrbit, block_list, nblk, blkbeglist, blkendlist, $
                         num_group, status

   FOR igrp=0L,num_group-1 DO BEGIN
      add_blk = 1
      noadd_blk = 1
      IF (igrp GT 0L) THEN noadd_blk = (blkbeglist[igrp] EQ blkendlist[igrp-1]) ? 1 : 0
      IF ((block_list[0] EQ 1 AND blkbeglist[igrp] EQ 1) OR noadd_blk) THEN add_blk = 0
      sub_blk = 1
      IF (block_list[nblk-1] EQ 180 AND blkendlist[igrp] EQ 180) THEN sub_blk = 0

      ndxs1 = WHERE(firepix_orbit[*].block GE blkbeglist[igrp]+add_blk AND $
                    firepix_orbit[*].block LE blkendlist[igrp]-sub_blk, numndxs1)
      IF (numndxs1 GT 0) THEN BEGIN
         firepix_group = firepix_orbit[ndxs1]
         GoodGroups[nGroups].orbit = orbitnum
         GoodGroups[nGroups].blk_beg = blkbeglist[igrp]
         GoodGroups[nGroups].blk_end = blkendlist[igrp]
         GoodGroups[nGroups].num_pix = numndxs1
         GoodGroups[nGroups].tot_power = ROUND(TOTAL(firepix_group[*].mean_power))

         ;------------------------------------------------------------------
         ; Write successful block-group data to the "Process" file.
         ;------------------------------------------------------------------

         PRINTF, Unit1, FORMAT='(I5,1X,2I5,A14,I6,A)', orbitnum, $
                        blkbeglist[igrp], blkendlist[igrp], $
                        firepix_orbit[0].date, $
                        ROUND(GoodGroups[nGroups].tot_power), ' MWatt'

         nGroups += 1
         IF (nGroups GE MAX_GROUPS) THEN BEGIN
            mssg = ['The maximum number of successful block ' + $
                    'groups allowed in one run is ' + $
                    STRTRIM(STRING(MAX_GROUPS),2), $
                    'Reduce the size of this run and try again.']
            rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
            RETURN
         ENDIF
      ENDIF

      ;---------------------------------------------------------------------
      ; On blocks.
      ;---------------------------------------------------------------------

      FOR iblk=blkbeglist[igrp],blkendlist[igrp]-1 DO BEGIN
         ndxs1 = WHERE(firepix_group[*].block EQ iblk, numndxs1)
         IF (numndxs1 GT 0) THEN BEGIN
            firepix_block = firepix_group[ndxs1]
            GoodBlocks[nBlocks].orbit = orbitnum
            GoodBlocks[nBlocks].block = iblk
            GoodBlocks[nBlocks].num_pix = numndxs1
            GoodBlocks[nBlocks].tot_power = ROUND(TOTAL(firepix_block[*].mean_power))
            nBlocks += 1
            IF (nBlocks GE MAX_BLOCKS) THEN BEGIN
               mssg = ['The maximum number of successful MISR ' + $
                       'blocks allowed in one run is ' + $
                       STRTRIM(STRING(MAX_BLOCKS),2), $
                       'Reduce the size of this run and try again.']
               rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
               RETURN
           ENDIF
         ENDIF
      ENDFOR
   ENDFOR

ENDFOR

GoodOrbits = GoodOrbits[0:nOrbits-1]
GoodGroups = GoodGroups[0:nGroups-1]
GoodBlocks = GoodBlocks[0:nBlocks-1]

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

firepix_orbit = 0
firepix_group = 0
firepix_block = 0
good_firepix = 0
new_orbit_list = 0
orbit_list = 0
block_list = 0
ndxs1 = 0

Retval = 0

END  ;  WriteMINXFireData

;***************************************************************************
PRO ProcessModisFirePixels, DefaultProjDir
;***************************************************************************
; Program reads relevant fire pixel data from MODIS MOD14 thermal anomaly
; files and writes a filtered version out to a file to be read by MINX.
; The output data includes latitude/longitude, MISR block/line/samp, fire
; radiative power in megawatts, reflectivity, 4 channels of brightness
; temperatures and the confidence level.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
; These are for testing only. 
;---------------------------------------------------------------------------

SAVED_WRITE = 1
SAVED_READ  = 2

;set the following to 0 for production
SAVED_MOD14_DATA = 0  ; 0 => do not save fire pixel data for reuse here
                      ; 1 => save fire pixel data for test
                      ; 2 => read saved fire pixel data for test
SHOW_HISTOGRAMS = 0   ; 0 => do not generate histograms of filtering effects
                      ; 1 => generate and display histograms
RUN_LIKE_BATCH = 0    ; 0 => show dialog boxes that interrupt processing
                      ; 1 => do not show dialog boxes

;---------------------------------------------------------------------------
; Initialize constants and variables.
;---------------------------------------------------------------------------

exten = '*.txt'
wild_card = '*'
buff = ''
num_modis = 0L
Status = -1

;---------------------------------------------------------------------------
; Select an existing project directory or create a new one. GetLastFilename
; autmatically updates the !VAR variable to point to the new directory.
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypeUtilProjDir, '*', 0, DefaultProjDir, $
                 dummy
IF (DefaultProjDir EQ '') THEN RETURN

IF (~ FILE_TEST(DefaultProjDir, /DIRECTORY)) THEN BEGIN
   rtrn_val = MakeDirectory(DefaultProjDir)
   rtrn_val = ChmodCatchError(DefaultProjDir, '777'O)
ENDIF

!SAV.DfltFiles[!KON.FileTyp.TypeMOD14dir].SavePath = DefaultProjDir

;---------------------------------------------------------------------------
; Have the user select the directory containing the MODIS granules that
; were downloaded earlier.
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypeMOD14dir, '*', 0, Mod14GranuleDir, dummy
IF (Mod14GranuleDir EQ '') THEN RETURN

;---------------------------------------------------------------------------
; Ask the user for some parameters: the first and last MISR block and path
; numbers to consider valid for these data, the number of fire pixels there
; must be in a MISR orbit to use that orbit, the max number of blocks the
; users OS can handle at once, and the MODIS confidence threshold for
; accepting fire pixels.
;---------------------------------------------------------------------------

GetMOD14Params_gui, cancel

IF (cancel EQ 1) THEN RETURN

;---------------------------------------------------------------------------
; Create names of files and subdirectories to be generated.
;---------------------------------------------------------------------------

out_order_file = DefaultProjDir + 'MisrOrderList_MOD14_' + $
                 !SAV.Util.ProjectName + '.txt'
            
out_process_file = DefaultProjDir + 'MisrProcessList_MOD14_' + $
                   !SAV.Util.ProjectName + '.txt'
            
out_pixel_file = 'FirePixels_MOD14__' + !SAV.Util.ProjectName + '.txt'
            
out_log_file = DefaultProjDir + 'FirePixReport_MOD14_' + $
               !SAV.Util.ProjectName + '.txt'
   
fail_file = DefaultProjDir + 'MOD14FailedFiles_' + $
            !SAV.Util.ProjectName + '.txt'
   
mod14_list_file = DefaultProjDir + 'MOD14_GranuleList_User_' + $
                  !SAV.Util.ProjectName + '.txt'

fire_pix_dir = DefaultProjDir + 'FirePixels_MOD14_' + $
               !SAV.Util.ProjectName + !KON.Misc.Slash
            
dig_output_dir = DefaultProjDir + 'Digitize_Output' + !KON.Misc.Slash

;---------------------------------------------------------------------------
; Initialize variables for testing a large collection of MODIS granules.
; This is so the large CPU burden isn't repeated for each test.
;---------------------------------------------------------------------------

IF (SAVED_MOD14_DATA) THEN BEGIN
   MOD14_save_dir = '~' + !KON.Misc.Slash + 'MOD14_Save'
   IF (~ FILE_TEST(MOD14_save_dir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(MOD14_save_dir)
      rtrn_val = ChmodCatchError(MOD14_save_dir, '777'O)
   ENDIF
   save_old_file = MOD14_save_dir + !KON.Misc.Slash + 'SaveOldMOD14Data_' + $
                   !SAV.Util.ProjectName
   save_hist_image_file = 'HistImage_' + !SAV.Util.ProjectName
ENDIF

;---------------------------------------------------------------------------
; Test the directory where output fire pixel files will be written. If there
; are files there, ask if they should be deleted. 
;---------------------------------------------------------------------------

IF (~ FILE_TEST(fire_pix_dir, /DIRECTORY)) THEN BEGIN
   rtrn_val = MakeDirectory(fire_pix_dir)
   rtrn_val = ChmodCatchError(fire_pix_dir, '777'O)
ENDIF

filenames = fire_pix_dir + 'FirePixels_*' + '.txt'
myfilelist = FILE_SEARCH(filenames)

IF (N_ELEMENTS(myfilelist) GT 0 AND myfilelist[0] NE '') THEN BEGIN
   mssg = ['There are already fire pixel files in this location:', filenames, '', $
           'For orbits being processed that already have a file in,' + $
           'this directory, new data will be appended to the', $
           'existing file.', $
           'Do you want to delete all the fire pixel files instead?']
   rval = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)
   IF (STRUPCASE(rval) EQ 'YES') THEN FILE_DELETE, myfilelist
ENDIF

myfilelist = 0

;---------------------------------------------------------------------------
; Create a file listing all the granule names.
;---------------------------------------------------------------------------

gran_list = FILE_SEARCH(Mod14GranuleDir + 'MOD14.*.hdf')
num_granules = N_ELEMENTS(gran_list)

IF (num_granules EQ 0 OR (num_granules EQ 1 AND gran_list[0] EQ '')) THEN BEGIN
   mssg = ['No MOD14 granules were found in the directory you selected:', $
           Mod14GranuleDir, 'Try again.']
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   RETURN
ENDIF

OPENW, unit, mod14_list_file, /GET_LUN
PRINTF, unit, gran_list
FREE_LUN, unit
gran_list = 0

!SAV.DfltFiles[!KON.FileTyp.TypeMOD14file].SavePath = mod14_list_file

;---------------------------------------------------------------------------
; Open the file containing the list of MODIS granules for which to get fire
; pixels and read the full path names of the granules.
;---------------------------------------------------------------------------

MOD14files = STRARR(num_granules)

OPENR, unit, mod14_list_file, /GET_LUN

WHILE (~EOF(unit)) DO BEGIN
   READF, unit, buff
   buff = STRTRIM(buff, 2)
   IF (STRMID(buff, 3, /REVERSE_OFFSET) NE '.hdf' OR buff EQ '') THEN CONTINUE
   MOD14files[num_modis] = buff
   num_modis += 1
ENDWHILE

FREE_LUN, unit

MOD14files = MOD14files[0:num_modis-1]
nMODfiles = N_ELEMENTS(MOD14files)

;---------------------------------------------------------------------------
; Create a no-blocking message to user that processing will take a while.
;---------------------------------------------------------------------------

baseID = WIDGET_BASE(RESOURCE_NAME='test', UNITS=0, $
   TITLE='Processing .....', $
   XOFFSET=!KON.Misc.ScreenX / 2, $
   YOFFSET=!KON.Misc.ScreenY / 2)
   
strNumgran = STRTRIM(STRING(nMODfiles),2)
   
mssg = [' ', '  MINX is processing ' + strNumgran + $
        ' MODIS fire pixel granules.', $
        '  Please be patient ....................................', ' ']
textID = WIDGET_TEXT(baseID, VALUE=mssg, /WRAP, XSIZE=60, YSIZE=4)
WIDGET_CONTROL, /REALIZE, baseID
XMANAGER, 'test', baseID, /NO_BLOCK
mssg = 0

;---------------------------------------------------------------------------
; Sort the filenames so they are in chronological (and thus orbit) order.
;---------------------------------------------------------------------------

sort_order = SORT(MOD14files)
MOD14files = MOD14files[sort_order[*]]
sort_order = 0

;---------------------------------------------------------------------------
; Convert the beginning and ending dates to Julian.
;---------------------------------------------------------------------------

GetJulianDay, !SAV.Util.BegDate, '00:00:00', 0, ymd, hms, jul_beg
GetJulianDay, !SAV.Util.EndDate, '23:59:59', 0, ymd, hms, jul_end

;---------------------------------------------------------------------------
; Initialize the counters for reasons why pixels were not used while reading
; from the MODIS granules. These are based on geographic and time factors.
;---------------------------------------------------------------------------

nTotalFirePix  = 0L
nGoodFirePix   = 0L
ntot_err_fail  = 0L
ntot_path_fail = 0L
ntot_date_fail = 0L
ntot_day_fail  = 0L
ntot_blk_fail  = 0L
ntot_samp_fail = 0L
ntot_pixcnf_fail = 0L
ntot_pixpow_fail = 0L

;---------------------------------------------------------------------------
; Get all the fire pixels inside files. 
;---------------------------------------------------------------------------

IF (SAVED_MOD14_DATA EQ SAVED_READ) THEN BEGIN

   ;------------------------------------------------------------------------
   ; If data have been saved previously to avoid very large processing times,
   ; read the saved results and proceed (can save minutes).
   ;------------------------------------------------------------------------

   OPENR, unitX, save_old_file + '.txt', /GET_LUN
   READF, unitX, FORMAT='(8I10)', nTotalFirePix, nGoodFirePix, $
                 ntot_err_fail, ntot_path_fail, ntot_date_fail, $
                 ntot_day_fail, ntot_blk_fail, ntot_samp_fail
   FREE_LUN, unitX
   RESTORE, save_old_file + '.sav'

ENDIF ELSE BEGIN

   ;------------------------------------------------------------------------
   ; Count all the fire pixels.
   ;------------------------------------------------------------------------

   max_fire_pixels = 0L

   FOR ifile=0L,nMODfiles-1 DO BEGIN
      CATCH, error_status
      IF (error_status NE 0) THEN GOTO, skip_file
      sd_id = HDF_StartInterface(MOD14files[ifile])
      IF (sd_id LE 0) THEN GOTO, skip_file
      ReadATTR, MOD14files[ifile], unit0, sd_id, 'FirePix', num_fire_pix, file_ok
      IF (file_ok LT 0) THEN GOTO, skip_file
      max_fire_pixels += num_fire_pix
skip_file:
      IF (sd_id GT 0) THEN HDF_SD_END, sd_id
      CATCH, /CANCEL
   ENDFOR

   ;------------------------------------------------------------------------
   ; Create a structure to contain MOD14 data for 1 fire pixel. Replicate it
   ; to hold all fire pixels in entire project that pass filtering tests!
   ;------------------------------------------------------------------------
   
   FirePixStruct = { latitude :       0.0D, $
                     longitude :      0.0D, $
                     date :             '', $
                     orbit :            0L, $
                     block :             0, $
                     MISRsamp :          0, $
                     MISRline :          0, $
                     mean_power :      0.0, $
                     reflectance_R2 :  0.0, $
                     brght_temp_T21 :  0.0, $
                     brght_temp_T31 :  0.0, $
                     brght_temp_mT21 : 0.0, $
                     brght_temp_mT31 : 0.0, $
                     fp_confidence :     0  }
   
   IF (max_fire_pixels LT 1) THEN BEGIN
      mssg = ['No fire pixels were found. Check that your list of MOD14', $
              'granules is correct and that the values you entered in the', $
              '"Fire Pixel Filter Parameters" dialog box are correct.']
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      FirePixStruct = 0
      WIDGET_CONTROL, baseID, /DESTROY
      RETURN
   ENDIF
   
   FirePixList = REPLICATE(FirePixStruct, max_fire_pixels)
   FirePixStruct = 0
   
   ;------------------------------------------------------------------------
   ; Open a file for reporting files that could not be opened or read.
   ;------------------------------------------------------------------------
   
   OPENW, unit0, fail_file, /GET_LUN

   ;------------------------------------------------------------------------
   ; Loop over the MOD14 granules and get all the fire pixels inside. Filter
   ; out those pixels that don't satisfy the geographic and time filter
   ; specs set by the user. Fill the array of structures with one good fire
   ; pixel per structure, where the structure elements are the data that go
   ; into the output fire pixel files. 
   ;------------------------------------------------------------------------

   FOR ifile=0L,nMODfiles-1 DO $
      ReadMod14FireData, unit0, MOD14files[ifile], jul_beg, jul_end, orbitnum, $
                         pathnum, date, FirePixList, nTotalFirePix, nGoodFirePix, $
                         ntot_err_fail, ntot_path_fail, ntot_date_fail, $
                         ntot_day_fail, ntot_blk_fail, ntot_samp_fail, statval

   IF (ntot_err_fail EQ 0) THEN PRINTF, unit0, 'No files failed.'
   FREE_LUN, unit0
   MOD14files = 0

   IF (nGoodFirePix LT 1) THEN BEGIN
      mssg = 'No fire pixels were found in any MOD14 granule that satisfy criteria.'
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)
      FirePixList = 0
      WIDGET_CONTROL, baseID, /DESTROY
      RETURN
   ENDIF

   FirePixList = FirePixList[0:nGoodFirePix-1]
ENDELSE

;---------------------------------------------------------------------------
; Save to a file the essential data generated in ReadMod14FireData so it can
; be loaded without long waits during debugging.
;---------------------------------------------------------------------------

IF (SAVED_MOD14_DATA EQ SAVED_WRITE) THEN BEGIN
   OPENW, unitX, save_old_file + '.txt', /GET_LUN
   PRINTF, unitX, FORMAT='(8I10)', nTotalFirePix, nGoodFirePix, $
           ntot_err_fail, ntot_path_fail, ntot_date_fail, ntot_day_fail, $
           ntot_blk_fail, ntot_samp_fail

   FREE_LUN, unitX
   SAVE, FirePixList, FILE=save_old_file + '.sav'
ENDIF

;---------------------------------------------------------------------------
; Open the files to contain the orbit "Process" and orbit "Order" lists and
; construct the fire pixel template name (__ will be changed to _<orbit>_).
;---------------------------------------------------------------------------

OPENW, unit1, out_process_file, /GET_LUN
OPENW, unit2, out_order_file, /GET_LUN

IF (~RUN_LIKE_BATCH) THEN $
   InsertHeaderRecords, unit1, $
                        !SAV.DfltFiles[!KON.FileTyp.TypeL1B2Terrain].SavePath, $
                        'F03_0024', dig_output_dir

;---------------------------------------------------------------------------
; Get the total number of fire pixels, the total power and the total number
; of orbits in pre-filtered data.
;---------------------------------------------------------------------------

TotalPixels = N_ELEMENTS(FirePixList)
TotalPower  = ROUND(TOTAL(FirePixList[*].mean_power))
orbits      = REFORM(FirePixList[*].orbit)
TotalOrbit  = N_ELEMENTS(orbits[UNIQ(orbits, SORT(orbits))])
orbits = 0

;---------------------------------------------------------------------------
; Process the fire pixels that passed the first filtering stage. Collect the
; pixels into orbits and block groups. Filter out block-groups that have too
; few pixels or too small a cumulative fire power. Write the fire pixel data
; to their orbit files.
;---------------------------------------------------------------------------

WriteMINXFireData, unit1, unit2, fire_pix_dir, out_pixel_file, FirePixList, $
                   GoodFirepix, GoodBlocks, GoodGroups, GoodOrbits, $
                   ntot_pixcnf_fail, ntot_pixpow_fail, Retval
FirePixList = 0
FREE_LUN, unit1

IF (Retval LT 0) THEN BEGIN
   FREE_LUN, unit2
   WIDGET_CONTROL, baseID, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Close files, clean up and set group permissions to rw.
;---------------------------------------------------------------------------

POINT_LUN, -unit2, end_pos
IF (end_pos GT 0) THEN BEGIN
   POINT_LUN, unit2, end_pos-1
   WRITEU, unit2, STRING(13b)
ENDIF
FREE_LUN, unit2

rtrn_val = ChmodCatchError(out_process_file, '666'O)
rtrn_val = ChmodCatchError(out_order_file,   '666'O)

;---------------------------------------------------------------------------
; Destroy the message window re. "processing is proceeding".
;---------------------------------------------------------------------------

WIDGET_CONTROL, baseID, /DESTROY

;---------------------------------------------------------------------------
; Print a report in a message box.
;---------------------------------------------------------------------------

num_reinstate = ABS(nTotalFirePix  - ntot_err_fail    - ntot_path_fail   - $
                    ntot_date_fail - ntot_day_fail    - ntot_blk_fail    - $
                    ntot_samp_fail - ntot_pixcnf_fail - ntot_pixpow_fail - $
                    N_ELEMENTS(GoodFirepix))

fmt1 = '(I12,A)'
fmt2 = '(2x,A10,A)'

s0  = STRING(FORMAT='(A)', 'Project "' + !SAV.Util.ProjectName + '"')
s1  = STRING(FORMAT='(A)', 'Fire Pixel Statistics Based on MODIS MOD14 Data')
s2  = STRING(FORMAT='(A)', 'Generated ' + SYSTIME() + ' by ' + !KON.Misc.UserName)
s3  =   ' '
s4  = STRING(FORMAT='(A)', 'User Input Parameters:')
s5  = STRING(FORMAT=fmt1,  !SAV.Util.FirstValidBlock,' = First valid block')
s6  = STRING(FORMAT=fmt1,  !SAV.Util.LastValidBlock, ' = Last valid block')
s7  = STRING(FORMAT=fmt1,  !SAV.Util.FirstValidPath, ' = First valid path')
s8  = STRING(FORMAT=fmt1,  !SAV.Util.LastValidPath,  ' = Last valid path')
s9  = STRING(FORMAT=fmt2,  !SAV.Util.BegDate,        ' = Begin date to process')
s10 = STRING(FORMAT=fmt2,  !SAV.Util.EndDate,        ' = End date to process')
s11 = STRING(FORMAT=fmt1,  !SAV.Util.MaxBlksPerLoad, $
                           ' = Maximum number of blocks for MINX to load')
s12 = STRING(FORMAT=fmt1,  !SAV.Util.ConfThresh, $
                           ' = Minimum acceptable MODIS confidence percent')
s13 = STRING(FORMAT=fmt1,  !SAV.Util.MinPixPower, $
                           ' = Minimum acceptable MODIS power per pixel')
s14 = STRING(FORMAT=fmt1,  !SAV.Util.MinBlkPower, $
                           ' = Minimum acceptable MODIS power per block')
s15 = STRING(FORMAT=fmt1,  !SAV.Util.MinGrpPower, $
                           ' = Minimum acceptable MODIS power per block-group')
s16 = STRING(FORMAT=fmt1,  !SAV.Util.NumFirePixGroup, $
                           ' = Minimum number of fire pixels per block-group')
s17 = STRING(FORMAT=fmt1,  nTotalFirePix, ' = Total number of fire pixels found')
s18 =   ' '
s19 = STRING(FORMAT='(A)', 'Number of MODIS fire pixels rejected and reasons:')
s20 = STRING(FORMAT=fmt1,  ntot_err_fail,    ' = Error encountered processing a pixel.')
s21 = STRING(FORMAT=fmt1,  ntot_path_fail,   ' = MISR path outside requested range')
s22 = STRING(FORMAT=fmt1,  ntot_date_fail,   ' = Pixel outside requested date range')
s23 = STRING(FORMAT=fmt1,  ntot_day_fail,    ' = Fire pixel on MODIS night side')
s24 = STRING(FORMAT=fmt1,  ntot_blk_fail,    ' = MISR block outside requested range')
s25 = STRING(FORMAT=fmt1,  ntot_samp_fail,   ' = Pixel on no-data edge of MISR swath')
s26 = STRING(FORMAT=fmt1,  ntot_pixcnf_fail, ' = MODIS confidence level too low (pass 1)')
s27 = STRING(FORMAT=fmt1,  ntot_pixpow_fail, ' = Radiative power too low in pixel (pass 1)')
s28 = ' '
s29 = STRING(FORMAT='(A)', 'Number of fire pixels reinstated in pass 2:')
s30 = STRING(FORMAT=fmt1,  num_reinstate,    ' = Associated with accepted fire pixels in a block')
s31 = ' '
s32 = STRING(FORMAT='(A)', 'Accepted Data:')
s33 = STRING(FORMAT=fmt1,  N_ELEMENTS(GoodFirepix),' = Number of MODIS fire pixels accepted')
s34 = STRING(FORMAT=fmt1,  N_ELEMENTS(GoodOrbits), ' = Number of MISR orbits to order')
s35 = STRING(FORMAT=fmt1,  N_ELEMENTS(GoodGroups), ' = Number of MISR block-groups to process')
s36 = ' '
s37 = 'To order MISR orbits:'
s38 = '  1) go to this website:'
s39 = '     ' + !SAV.MISROrderWebsite
s40 = '  2) use the orbit list in this file:'
s41 = '     ' + out_order_file

mssg = [ s0, s1, s2, s3, s4, s5, s6, s7, s8, s9,s10,s11,s12,s13,s14,s15,s16, $
        s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,$
        s34,s35,s36, s37,s38,s39,s40,s41]

IF (~RUN_LIKE_BATCH) THEN rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

;---------------------------------------------------------------------------
; Print the same report to a log file and make sure file permissions are set
; to group rw.
;---------------------------------------------------------------------------

OPENW, unit, out_log_file, /GET_LUN
FOR ilin=0,N_ELEMENTS(mssg)-1 DO PRINTF, unit, mssg[ilin]
mssg = 0
FREE_LUN, unit

rtrn_val = ChmodCatchError(out_log_file, '666'O)

;---------------------------------------------------------------------------
; If needed, create histograms for:
;  1) confidence per pixel
;  2) fire power per pixel
;  3) fire power per block
;  4) fire power per block-group
;  5) fire power per block-group
;  6) fire power per orbit
;---------------------------------------------------------------------------

IF (SHOW_HISTOGRAMS) THEN BEGIN

   num_firepix = N_ELEMENTS(GoodFirepix)
   num_blocks  = N_ELEMENTS(GoodBlocks)
   num_groups  = N_ELEMENTS(GoodGroups)
   num_orbits  = N_ELEMENTS(GoodOrbits)

   ;------------------------------------------------------------------------
   ; Pull out the required data into arrays for plotting.
   ;------------------------------------------------------------------------

   PixCnf = GoodFirepix[*].fp_confidence
   PixPow = GoodFirepix[*].mean_power
   BlkPow = GoodBlocks[*].tot_power
   GrpPow = GoodGroups[*].tot_power
   GrpNum = GoodGroups[*].num_pix
   OrbPow = GoodOrbits[*].tot_power

   NumPixCnf = num_firepix
   NumPixPow = num_firepix
   NumBlkPow = num_blocks
   NumGrpPow = num_groups
   NumGrpNum = num_groups
   NumOrbPow = num_orbits

   ;------------------------------------------------------------------------
   ; Set up image-specific parameters.
   ;------------------------------------------------------------------------

   DelBin = [1.0, 0.5, 3.0, 20.0, 1.0, 20.0]
   NumBin = [100, 100, 100, 100,  100,  100]
   NumBin += 1

   Titles = ['Confidence Level per Pixel', 'Radiative Power per Pixel', $
             'Radiative Power per Block',  'Radiative Power per Block-Group', $
             'Num Pixels per Block-group', 'Radiative Power per Orbit']
   ShrtTitl = ['ConfLevel', 'PowPerPix', 'PowPerBlk', 'PowPerGrp', $
               'PixPerGrp', 'PowPerOrb']
   Titles = !SAV.Util.ProjectName + ' Fire Pixels - ' + Titles

   XTitles = ['Confidence in %',       'Megawatts per Pixel', $
              'Megawatts per Block',   'Megawatts per Block-Group', $
              'Number of Pixels',      'Megawatts per Orbit']
   YTitles = ['Pixels per Bin',        'Pixels per Bin', $
              'Blocks per Bin',        'Block-Groups per Bin', $
              'Block-Groups per Bin',  'Orbits per Bin']

   XYSTout = ['num pixels > ',         'num pixels > ', $
              'num blocks > ',         'num blk-grps > ', $
              'num blk-grps > ',       'num orbits > ']

   ;---------------------------------------------------------------------
   ; Create filter parameter text line.
   ;---------------------------------------------------------------------

   filter_parms = $
         'BlkPerGrp=' + STRTRIM(STRING(!SAV.Util.MaxBlksPerLoad),2) + $
       ', ConfThrsh=' + STRTRIM(STRING(ROUND(!SAV.Util.ConfThresh )),2) + $
       ', MinPixPow=' + STRTRIM(STRING(ROUND(!SAV.Util.MinPixPower)),2) + $
       ', MinBlkPow=' + STRTRIM(STRING(ROUND(!SAV.Util.MinBlkPower)),2) + $
       ', MinGrpPow=' + STRTRIM(STRING(ROUND(!SAV.Util.MinGrpPower)),2) + $
       ', NumPixGrp=' + STRTRIM(STRING(!SAV.Util.NumFirePixGroup),2)

   ;---------------------------------------------------------------------
   ; Create directory for saved histograms.
   ;---------------------------------------------------------------------

   hist_dir = DefaultProjDir + 'MOD14_Hist' + !KON.Misc.Slash
   IF (~ FILE_TEST(hist_dir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(hist_dir)
      rtrn_val = ChmodCatchError(hist_dir, '777'O)
   ENDIF

   ;---------------------------------------------------------------------
   ; Plot the images - save in .png files.
   ;---------------------------------------------------------------------

   FOR ihist=0,N_ELEMENTS(NumBin)-1 DO BEGIN
      TempData = (ihist EQ 0) ? PixCnf : ((ihist EQ 1) ? PixPow : $
                ((ihist EQ 2) ? BlkPow : ((ihist EQ 3) ? GrpPow : $
                ((ihist EQ 4) ? GrpNum : OrbPow))))

      NumData  = (ihist EQ 0) ? NumPixCnf : ((ihist EQ 1) ? NumPixPow : $
                ((ihist EQ 2) ? NumBlkPow : ((ihist EQ 3) ? NumGrpPow : $
                ((ihist EQ 4) ? NumGrpNum : NumOrbPow))))

      hist_counts = HISTOGRAM(TempData[0:NumData-1], MIN=0.0, $
                              BINSIZE=DelBin[ihist], NBINS=NumBin[ihist])
      bins = FINDGEN(N_ELEMENTS(hist_counts) + 1) * DelBin[ihist]

      WINDOW, 5, TITLE=Titles[ihist], XPOS=200, YPOS=200, XSIZE=1100, YSIZE=700
      PLOT, [0.0,0.1], [0.0,0.0], TITLE=Titles[ihist], PSYM=10, CHARSIZE=2, $
            SUBTITLE=filter_parms, XTITLE=XTitles[ihist], YTITLE=YTitles[ihist], $
            BACKGROUND=16777215, COLOR=0, XTICKLEN=1, XGRIDSTYLE=1, $  
            XRANGE=[0.0,DelBin[ihist]*(NumBin[ihist]-1)], YTICKLEN=1, $  
            YRANGE=[0,MAX(hist_counts)*1.05], YGRIDSTYLE=1, XMARGIN=[10,3], $
            YMARGIN=[5,2]
      OPLOT, bins, hist_counts, PSYM=10, COLOR=16711680, THICK=2

      last_bin_end_val = bins[NumBin[ihist]-1] + DelBin[ihist]
      ndxs = WHERE(TempData[0:NumData-1] GE last_bin_end_val, numndxs)

      tot_power_out = ROUND(TOTAL(GoodFirepix[*].mean_power))
      val1  = STRTRIM(STRING(LONG(TotalOrbit)),2)
      val2  = STRTRIM(STRING(LONG(num_orbits)),2)
      val3  = STRTRIM(STRING(TotalPixels),2)
      val4  = STRTRIM(STRING(num_firepix),2)
      val4a = STRTRIM(STRING(ROUND(num_firepix*100/FLOAT(TotalPixels))),2)
      val5  = STRTRIM(STRING(ROUND(num_firepix/FLOAT(num_orbits))),2)
      val6  = STRTRIM(STRING(ROUND(num_firepix/FLOAT(num_groups))),2)
      val7  = STRTRIM(STRING(TotalPower),2)
      val8  = STRTRIM(STRING(tot_power_out),2)
      val8a = STRTRIM(STRING(ROUND(tot_power_out*100/FLOAT(TotalPower))),2)
      val9  = STRTRIM(STRING(LONG(num_groups)),2)
      val10 = STRING(FORMAT='(F5.2)',num_groups/FLOAT(num_orbits))
      val11 = STRTRIM(STRING(LONG(num_blocks)),2)
      val12 = STRING(FORMAT='(F4.1)',num_blocks/FLOAT(num_groups))
      val13 = STRTRIM(STRING(ROUND(last_bin_end_val)),2)
      val14 = STRTRIM(STRING(numndxs),2)

      xbeg = 550
      ycrd = 640
      XYOUTS, xbeg, ycrd, 'number orbits: in/out        =  ' + val1 + $
              ' / ' + val2, /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, 'number fire pixels: in/out/% =  ' + val3 + $
              ' / ' + val4 + ' / ' + val4a + '%', $
              /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, 'number fire pix: per-orb     =  ' + val5, $
              /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, '                 per-blk-grp =  ' + val6, $
              /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, 'fire power (MW): in/out/%    =  ' + val7 + $
              ' / ' + val8 + ' / ' + val8a + '%', $
              /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, 'num blk-grps: out/per-orbit =  ' + val9 + $
              ' /' + val10, /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, 'num blks: out/per-blk-grp   =  ' + val11 + $
              ' /' + val12, /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      ycrd -= 20
      XYOUTS, xbeg, ycrd, XYSTout[ihist] + val13 + '            = ' + $
              val14, /DEVICE, CHARSIZE=1.5, ALIGNMENT=0.0, COLOR=255
      hist_counts = 0

      ; Save image to a .png file.
      filename = hist_dir + (['A', 'B', 'C', 'D', 'E', 'F'])[ihist] + $
                 '_HistImage_' + ShrtTitl[ihist] + '.png'
      saveimage = TVRD(/ORDER, TRUE=1)
      WRITE_PNG, filename, saveimage, /ORDER
   ENDFOR

   mssg = ['Histograms have been saved to directory: ', hist_dir]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFORMATION)

   saveimage = 0

ENDIF

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

PixCnf = 0
PixPow = 0
BlkPow = 0
GrpPow = 0
GrpNum = 0
GoodFirepix = 0
GoodBlocks = 0
GoodGroups = 0
GoodOrbits = 0
TempData = 0

END  ;  ProcessModisFirePixels


;***************************************************************************
;***************************************************************************
PRO CreateMisrOrderScript, DefaultDir
;***************************************************************************
; NOT IMPLEMENTED YET - This could be an additional important 
; reason to use MINX. Call it from OverpassFinder, from the
; ModVolc and MODIS procedures above and perhaps place it on the
; main menu as an independent option.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------



END  ;  CreateMisrOrderScript


;***************************************************************************
;***************************************************************************
PRO ReplaceTemplateLine, LineIn, Alias, Name, VerNum, BlkRange
;***************************************************************************
; Replace the dummy value in a line of text with a real value.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

BlkRange = ''

;---------------------------------------------------------------------------
; Determine if the placeholder is in this line of text. Check for as many as
; 3 occurrences.
;---------------------------------------------------------------------------

FOR irep=0,2 DO BEGIN

   name_sub = Name
   ipos = STRPOS(LineIn, Alias)

   ;------------------------------------------------------------------------
   ; If found, replace the placeholder with appropriate text.
   ;------------------------------------------------------------------------

   IF (ipos GE 0) THEN BEGIN

      IF (irep EQ 0 AND Alias EQ 'xPROLOGx') THEN BEGIN

         ;------------------------------------------------------------------
         ; If there is a subsetted block list in name, then we must account
         ; for the possibility that the range is different than we expect.
         ; This can happen at the beginning or the end in high latitudes due
         ; to seasonal variation in the valid blocks, not at both ends.
         ;------------------------------------------------------------------

         ipos1 = STRPOS(name_sub, '.b', /REVERSE_SEARCH)
         IF (ipos1 GT (-1)) THEN BEGIN
            version = STRMID(name_sub, 0, ipos1)
            BlkRange = STRMID(name_sub, ipos1+2)
            name_sub = '*.b*'
         ENDIF ELSE BEGIN
            version = STRMID(name_sub, 0)
            name_sub = '*'
         ENDELSE

         ;------------------------------------------------------------------
         ; Get the version number from the version string.
         ;------------------------------------------------------------------

         ipos1 = STRPOS(version, '_', /REVERSE_SEARCH)
         VerNum = FIX(STRMID(version, ipos1+1))

      ENDIF

      line_temp = STRMID(LineIn, 0, ipos)
      line_temp += name_sub
      ilen = STRLEN(LineIn)
      ipos += STRLEN(Alias)
      line_temp += STRMID(LineIn, ipos, ilen-1)
      LineIn = line_temp
   ENDIF

ENDFOR

END  ;  ReplaceTemplateLine

;***************************************************************************
PRO ProcessOrbits, Unit, OrbitList, NumOrbits, Template, PathAlias, $
                   OrbitAlias, PrologAlias, NewProlog, MinSize, Description, $
                   Underscores
;***************************************************************************
; Process the orbits that were ordered and determine for each if it is 
; available.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

orbit_valid   = LONARR(NumOrbits)
files_valid   = STRARR(NumOrbits)
files_missing = STRARR(NumOrbits)
files_small   = STRARR(NumOrbits)
paths_missing = STRARR(NumOrbits)
nvalid   = 0
nmissing = 0
nsmall   = 0
nexpect  = 0

agp_str_pos = STRPOS(Description, 'AGP')
IF (agp_str_pos GT 0) THEN BEGIN
   paths_valid  = STRARR(NumOrbits)
   paths_expect = STRARR(NumOrbits)
ENDIF

;---------------------------------------------------------------------------
; Loop over the orbits and determine the status of each.
;---------------------------------------------------------------------------

FOR iorbit=0,NumOrbits-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Compute the path number from the orbit number.
   ;------------------------------------------------------------------------

   orbitnum = LONG(OrbitList[iorbit])
   orbitstr = STRTRIM(STRING(orbitnum),2)
   IF (STRLEN(orbitstr) LT 5) THEN orbitstr = '0' + orbitstr
   IF (STRLEN(orbitstr) LT 5) THEN orbitstr = '0' + orbitstr

   pathnum = PathFromOrbit(orbitnum)
   pathstr = STRTRIM(STRING(pathnum),2)
   IF (STRLEN(pathstr) LT 3) THEN pathstr = '0' + pathstr
   IF (STRLEN(pathstr) LT 3) THEN pathstr = '0' + pathstr

   ;------------------------------------------------------------------------
   ; Create the file name from the template and the path/orbit.
   ;------------------------------------------------------------------------

   numtemp = N_ELEMENTS(Template)

   FOR itemp=0,numtemp-1 DO BEGIN

      temp = Template[itemp]
      ReplaceTemplateLine, temp, PathAlias,   pathstr,   vernum, brange
      ReplaceTemplateLine, temp, OrbitAlias,  orbitStr,  vernum, brange
      ReplaceTemplateLine, temp, PrologAlias, NewProlog, vernum, brange

      verblks2 = STRSPLIT(brange, '-', /EXTRACT)

      ;---------------------------------------------------------------------
      ; Test if the file with the correct block range exists. We can't do
      ; this exactly. Also test if the file has a version number >= the
      ; number in the sample file.
      ;---------------------------------------------------------------------

      IF (agp_str_pos GT 0) THEN BEGIN
         paths_expect[nexpect] = pathstr
         nexpect += 1
      ENDIF

      filelist = FILE_SEARCH(temp)

      IF (filelist[0] NE '') THEN BEGIN
         nfile = N_ELEMENTS(filelist)
         found1 = 0

         FOR ifile=0,nfile-1 DO BEGIN
            ipos1 = STRPOS(filelist[ifile], '.b',     /REVERSE_SEARCH)
            ipos2 = STRPOS(filelist[ifile], '.hdf',   /REVERSE_SEARCH)
            ipos3 = STRPOS(filelist[ifile], '_F',     /REVERSE_SEARCH)
            ipos4 = STRPOS(filelist[ifile], '.subset',/REVERSE_SEARCH)
            version = STRMID(filelist[ifile], ipos3+1)
            dotpos = ipos1
            IF (dotpos LT 0) THEN dotpos = ipos2
            ipos5 = STRPOS(version, '_',   /REVERSE_SEARCH)
            version_num = FIX(STRMID(version, ipos5+1))

            ;---------------------------------------------------------------
            ; Test if the file has a version number >= the number in the
            ; template. If it does, make sure that an orbit file with a 
            ; different version has not already been used. Count only once.
            ; Reject any files with the 'subset' designation.
            ;---------------------------------------------------------------

            IF (version_num GE vernum) THEN BEGIN
               IF (ipos1 LT 0) THEN BEGIN
                  IF (ipos4 LT 0) THEN BEGIN
                     ndxs = WHERE(orbitnum EQ orbit_valid, num_ndxs)
                     IF (num_ndxs EQ 0) THEN BEGIN
                        files_valid[nvalid] = filelist[ifile]
                        orbit_valid[nvalid] = orbitnum
                        IF (agp_str_pos GT 0) THEN paths_valid[nvalid] = pathstr
                        nvalid += 1
                        found1 = 1
                     ENDIF
                  ENDIF
               ENDIF
               IF (brange NE '' AND ipos1 GE 0) THEN BEGIN
                  verblk = STRMID(filelist[ifile], ipos1+2, ipos2-ipos1-2)
                  verblks1 = STRSPLIT(verblk, '-', /EXTRACT)
                  IF (N_ELEMENTS(verblks1) EQ 2) THEN BEGIN
                     IF (verblks1[0] EQ verblks2[0] OR $
                         verblks1[1] EQ verblks2[1]) THEN BEGIN
                        files_valid[nvalid] = filelist[ifile]
                        IF (agp_str_pos GT 0) THEN $
                           paths_valid[nvalid] = pathstr
                        nvalid += 1
                        found1 = 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDFOR
         IF (~ found1) THEN BEGIN
            files_missing[nmissing] = temp
            nmissing += 1
         ENDIF
      ENDIF ELSE BEGIN
         IF (itemp EQ numtemp-1) THEN BEGIN
            files_missing[nmissing] = temp
            nmissing += 1
            BREAK
         ENDIF
      ENDELSE

   ENDFOR
ENDFOR

ndxs = 0

;---------------------------------------------------------------------------
; Post-process the AGP files for which there is only one/path. Find the
; unique paths among both the found and missing files.
;---------------------------------------------------------------------------

num_expect = STRTRIM(STRING(NumOrbits),2)
orbit_path = ' orbits :'

IF (agp_str_pos GT 0) THEN BEGIN
   ndxs = WHERE(paths_valid NE '', numndxs)
   IF (numndxs GT 0) THEN paths_valid = paths_valid[ndxs]
   ndxs = WHERE(paths_expect NE '', numndxs)
   IF (numndxs GT 0) THEN paths_expect = paths_expect[ndxs]
   uniq1 = paths_valid[UNIQ(paths_valid, SORT(paths_valid))]
   uniq2 = paths_expect[UNIQ(paths_expect, SORT(paths_expect))]
   nvalid  = N_ELEMENTS(uniq1)
   nexpect = N_ELEMENTS(uniq2)
   num_expect = STRTRIM(STRING(nexpect),2)
   nmissing = nexpect - nvalid
   ndiff = 0
   FOR ipath=0,nexpect-1 DO BEGIN
      ndxs = WHERE(paths_valid EQ paths_expect[ipath], numndx)
      IF (numndx LE 0) THEN BEGIN
         paths_missing[ndiff] = paths_expect[ipath]
         ndiff += 1
      ENDIF
   ENDFOR
   IF (ndiff GE 1) THEN BEGIN
      paths_missing = paths_missing[0:ndiff-1]
   ENDIF ELSE BEGIN
      paths_missing = ['']
   ENDELSE
   uniq1 = 0
   uniq2 = 0
   orbit_path = ' paths :'
ENDIF

ndxs = 0

;---------------------------------------------------------------------------
; Print the results to the log file.
;---------------------------------------------------------------------------

PRINTF, Unit, ''
PRINTF, Unit, Underscores
PRINTF, Unit, Description
PRINTF, Unit, Underscores
PRINTF, Unit, 'Number of files expected = ' + num_expect
PRINTF, Unit, 'Number of files found    = ' + STRTRIM(STRING(nvalid),2)
PRINTF, Unit, 'Number of files missing  = ' + STRTRIM(STRING(nmissing),2)
;PRINTF, Unit, 'Number of small files    = ' + STRTRIM(STRING(nsmall),2)

IF (agp_str_pos LT 0) THEN BEGIN
   IF (nvalid GT 0) THEN BEGIN
      PRINTF, Unit, ''
      PRINTF, Unit, 'Files are valid for these ' + $
                    STRTRIM(STRING(nvalid),2) + ' orbits :'
      valid_orbits = STRARR(nvalid)
      FOR ivalid=0,nvalid-1 DO BEGIN
         ipos = STRPOS(files_valid[ivalid], '_O0')
         valid_orbits[ivalid] = STRMID(files_valid[ivalid], ipos+3, 5)
      ENDFOR
      PRINTF, Unit, FORMAT='(10(A,1x))', valid_orbits
   ENDIF
ENDIF

IF (nmissing GT 0) THEN BEGIN
   PRINTF, Unit, ''
   PRINTF, Unit, 'Files are missing for these ' + $
               STRTRIM(STRING(nmissing),2) + orbit_path
   missing_orbits = STRARR(nmissing)
   IF (agp_str_pos GT 0) THEN BEGIN
      PRINTF, Unit, FORMAT='(10(A,1x))', paths_missing
   ENDIF ELSE BEGIN
      FOR imiss=0,nmissing-1 DO BEGIN
         ipos = STRPOS(files_missing[imiss], '_O0')
         missing_orbits[imiss] = STRMID(files_missing[imiss], ipos+3, 5)
      ENDFOR
      PRINTF, Unit, FORMAT='(10(A,1x))', missing_orbits
   ENDELSE
ENDIF

IF (0) THEN BEGIN  ;  (nsmall GT 0) THEN BEGIN
   PRINTF, Unit, ''
   PRINTF, Unit, 'Files are too small for these ' + $
                 STRTRIM(STRING(nmissing),2) + ' orbits :'
   small_orbits = STRARR(nsmall)
   FOR ismall=0,nsmall-1 DO BEGIN
      ipos = STRPOS(files_small[ismall], '_O0')
      small_orbits[ismall] = STRMID(files_small[ismall], ipos+3, 5)
   ENDFOR
   PRINTF, Unit, FORMAT='(10(A,1x))', small_orbits
ENDIF

;---------------------------------------------------------------------------
; Clean up and return.
;---------------------------------------------------------------------------

files_valid    = 0
files_missing  = 0
files_small    = 0
valid_orbits   = 0
missing_orbits = 0
small_orbits   = 0

END  ; ProcessOrbits

;***************************************************************************
PRO GetDirAndTemplate, SampFile, FileTemplate, PathAlias, CamAlias, ProdDir, $
                       NewProdProlog, ProdTemplate
;***************************************************************************
; For a given product type, get the base directory where the files will be
; found and the version and block range, and then construct the template for
; the files. Allow the directory structure to be of the MISR ../bank/.. form,
; or allow all the files of a given product type to be in the same subdir,
; or they can be in 9 subdirectories named DF, BF, ... that are not of the
; MISR form.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

ss = !KON.Misc.Slash
subdir = [ss+'DF'+ss, ss+'CF'+ss, ss+'BF'+ss, ss+'AF'+ss, ss+'AN'+ss, $
          ss+'AA'+ss, ss+'BA'+ss, ss+'CA'+ss, ss+'DA'+ss]

npos = STRPOS(SampFile, ss, /REVERSE_SEARCH)
ProdDir  = STRMID(SampFile, 0, npos+1)
ProdFile = STRMID(SampFile, npos+1)

IF (STRMID(ProdDir, 0, 11) EQ ss + 'data' + ss + 'bank' + ss) THEN BEGIN
   npos = STRPOS(ProdDir, 'path', /REVERSE_SEARCH)
   ProdDir = STRMID(ProdDir, 0, npos) + 'path' + PathAlias + ss

   IF (STRPOS(ProdDir, 'GRP_TERRAIN') GT 0 OR $
       STRPOS(ProdDir, 'GRP_ELLIPSOID') GT 0) THEN ProdDir += CamAlias + ss
ENDIF ELSE BEGIN
   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
      npos = STRPOS(ProdDir, subdir[icam])
      IF (npos GE 0) THEN BEGIN
         IF (STRPOS(ProdDir, 'GRP_TERRAIN') GT 0 OR $
             STRPOS(ProdDir, 'GRP_ELLIPSOID') GT 0) THEN $
            ProdDir = STRMID(ProdDir, 0, npos+1) + CamAlias + ss
         BREAK
      ENDIF
   ENDFOR
ENDELSE

npos1 = STRPOS(ProdFile, '_F', /REVERSE_SEARCH)
npos2 = STRPOS(ProdFile, '.hdf', /REVERSE_SEARCH)
NewProdProlog = STRMID(ProdFile, npos1+1, npos2-npos1-1)
ProdTemplate = ProdDir + FileTemplate

END  ; GetDirAndTemplate

;***************************************************************************
PRO ProcessL1B2Files, Unit, Cameras, OrbitList, NumOrbits, L1B2Template, $
                      PathAlias, OrbitAlias, CameraAlias, PrologAlias, $
                      L1B2Prolog
;***************************************************************************
; Handle GRP_TERRAIN and GRP_ELLIPSOID files.
;---------------------------------------------------------------------------

files_valid   = STRARR(NumOrbits*9)
files_missing = STRARR(NumOrbits*9)
files_small   = STRARR(NumOrbits*9)
nvalid   = 0
nmissing = 0
nsmall   = 0

FOR iorbit=0,NumOrbits-1 DO BEGIN

   ;------------------------------------------------------------------------
   ; Compute the path number from the orbit number.
   ;------------------------------------------------------------------------

   orbitnum = LONG(OrbitList[iorbit])
   orbitstr = STRTRIM(STRING(orbitnum),2)
   IF (STRLEN(orbitstr) LT 5) THEN orbitstr = '0' + orbitstr
   IF (STRLEN(orbitstr) LT 5) THEN orbitstr = '0' + orbitstr

   pathnum = PathFromOrbit(orbitnum)
   pathstr = STRTRIM(STRING(pathnum),2)
   IF (STRLEN(pathstr) LT 3) THEN pathstr = '0' + pathstr
   IF (STRLEN(pathstr) LT 3) THEN pathstr = '0' + pathstr

   ;------------------------------------------------------------------------
   ; Loop over the cameras.
   ;------------------------------------------------------------------------

   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Create file name from the template and path/orbit/camera.
      ;---------------------------------------------------------------------

      numtemp = N_ELEMENTS(L1B2Template)

      FOR itemp=0,numtemp-1 DO BEGIN

         l1b2_temp = L1B2Template[itemp]
         ReplaceTemplateLine, l1b2_temp, PathAlias, pathstr, vernum, brange
         ReplaceTemplateLine, l1b2_temp, OrbitAlias, orbitstr, vernum, brange
         ReplaceTemplateLine, l1b2_temp, CameraAlias, Cameras[icam], vernum, brange
         ReplaceTemplateLine, l1b2_temp, PrologAlias, L1B2Prolog, vernum, brange

         verblks2 = STRSPLIT(brange, '-', /EXTRACT)

         ;------------------------------------------------------------------
         ; Test if the file with the correct block range exists. We can do
         ; this approximately. If an orbit was ordered with a block range,
         ; it's OK if the entire orbit is present. Also test if the file has
         ; a version number >= the number in the sample file.
         ;------------------------------------------------------------------

         filelist = FILE_SEARCH(l1b2_temp)

         IF (filelist[0] NE '') THEN BEGIN
            nfile = N_ELEMENTS(filelist)
            found1 = 0
            FOR ifile=0,nfile-1 DO BEGIN
               ipos1 = STRPOS(filelist[ifile], '.b',   /REVERSE_SEARCH)
               ipos2 = STRPOS(filelist[ifile], '.hdf', /REVERSE_SEARCH)
               ipos3 = STRPOS(filelist[ifile], '_F',   /REVERSE_SEARCH)
               version = STRMID(filelist[ifile], ipos3+1)
               dotpos = ipos1
               IF (dotpos LT 0) THEN dotpos = ipos2
               ipos4 = STRPOS(version, '_',   /REVERSE_SEARCH)
               version_num = FIX(STRMID(version, ipos4+1))
               IF (version_num EQ vernum) THEN BEGIN
                  IF (ipos1 LT 0) THEN BEGIN
                     files_valid[nvalid] = filelist[ifile]
                     nvalid += 1
                     found1 = 1
                  ENDIF
                  IF (brange NE '' AND ipos1 GE 0) THEN BEGIN
                     verblk = STRMID(filelist[ifile], ipos1+2, ipos2-ipos1-2)
                     verblks1 = STRSPLIT(verblk, '-', /EXTRACT)
                     IF (N_ELEMENTS(verblks1) EQ 2) THEN BEGIN
                        IF (verblks1[0] EQ verblks2[0] OR $
                            verblks1[1] EQ verblks2[1]) THEN BEGIN
                           files_valid[nvalid] = filelist[ifile]
                           nvalid += 1
                           found1 = 1
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDFOR
            IF (~ found1) THEN BEGIN
               files_missing[nmissing] = l1b2_temp
               nmissing += 1
            ENDIF
         ENDIF ELSE BEGIN
            IF (itemp EQ numtemp-1) THEN BEGIN
               files_missing[nmissing] = l1b2_temp
               nmissing += 1
               BREAK
            ENDIF
         ENDELSE

      ENDFOR

   ENDFOR
ENDFOR

;---------------------------------------------------------------------------
; Print the L1B2 results to the log file and clean up.
;---------------------------------------------------------------------------

npos = STRPOS(L1B2Template, '_GRP_TERRAIN_')
l1b2_text = (npos GE 0) ? 'GRP_TERRAIN' : 'GRP_ELLIPSOID'

PRINTF, Unit, ''
PRINTF, Unit, '-----------------------------------------------------------'
PRINTF, Unit, 'Order Status for ' + l1b2_text + ' Files for Version ' + L1B2Prolog
PRINTF, Unit, '-----------------------------------------------------------'
PRINTF, Unit, 'Number of orbits expected = ' + STRTRIM(STRING(NumOrbits),2)
PRINTF, Unit, 'Number of files expected  = ' + STRTRIM(STRING(NumOrbits*9),2)
PRINTF, Unit, 'Number of files found     = ' + STRTRIM(STRING(nvalid),2)
PRINTF, Unit, 'Number of files missing   = ' + STRTRIM(STRING(nmissing),2)
;PRINTF, Unit, 'Number of small files    = ' + STRTRIM(STRING(nsmall),2)

IF (nvalid GT 0) THEN BEGIN
   PRINTF, Unit, ''
   valid_orbits  = STRARR(nvalid)
   valid_cameras = STRARR(nvalid)
   orbit_save    = STRARR(nvalid)
   iorbitsave    = 0
   FOR ivalid=0,nvalid-1 DO BEGIN
      ipos = STRPOS(files_valid[ivalid], '_O0')
      valid_orbits[ivalid]  = STRMID(files_valid[ivalid], ipos+3, 5)
      valid_cameras[ivalid] = STRMID(files_valid[ivalid], ipos+9, 2)
   ENDFOR
   uniq_orbit_ndxs = UNIQ(valid_orbits, SORT(valid_orbits))
   nuniq = N_ELEMENTS(uniq_orbit_ndxs)
   FOR iuniq=0,nuniq-1 DO BEGIN
      ndxs = WHERE(valid_orbits EQ valid_orbits[uniq_orbit_ndxs[iuniq]], $
                   numndxs)
      IF (numndxs EQ 9) THEN BEGIN
         orbit_save[iorbitsave] = valid_orbits[uniq_orbit_ndxs[iuniq]]
         iorbitsave += 1
      ENDIF
   ENDFOR
   PRINTF, Unit, 'Files are valid for all 9 cameras for these ' + $
           STRTRIM(STRING(iorbitsave),2) + ' orbits :'
   IF (iorbitsave GT 0) THEN BEGIN
      orbit_save = orbit_save[0:iorbitsave-1]
      PRINTF, Unit, FORMAT='(10(A,1x))', orbit_save
   ENDIF
ENDIF

IF (nmissing GT 0) THEN BEGIN
   PRINTF, Unit, ''
   missing_orbits  = STRARR(nmissing)
   missing_cameras = STRARR(nmissing)
   FOR imiss=0,nmissing-1 DO BEGIN
      ipos = STRPOS(files_missing[imiss], '_O0')
      missing_orbits[imiss]  = STRMID(files_missing[imiss], ipos+3, 5)
      missing_cameras[imiss] = STRMID(files_missing[imiss], ipos+9, 2)
   ENDFOR
   uniq_orbit_ndxs = UNIQ(missing_orbits, SORT(missing_orbits))
   nuniq = N_ELEMENTS(uniq_orbit_ndxs)
   PRINTF, Unit, 'The following ' + STRTRIM(STRING(nmissing),2) + $
           ' camera files are missing for these ' + $
            STRTRIM(STRING(nuniq),2) + ' orbits :'
   FOR iuniq=0,nuniq-1 DO BEGIN
      ndxs = WHERE(missing_orbits EQ missing_orbits[uniq_orbit_ndxs[iuniq]])
      new_missing_cameras = STRARR(9)
      new_missing_cameras[0:N_ELEMENTS(ndxs)-1] = missing_cameras[ndxs]
      PRINTF, Unit, FORMAT='(A,A,9(A,1x))', $
              missing_orbits[uniq_orbit_ndxs[iuniq]], $
              ' : ', new_missing_cameras
      new_missing_cameras = 0
   ENDFOR
ENDIF

IF (0) THEN BEGIN  ;  (nsmall GT 0) THEN BEGIN
   PRINTF, Unit, ''
   small_orbits  = STRARR(nsmall)
   small_cameras = STRARR(nsmall)
   FOR ismall=0,nsmall-1 DO BEGIN
      ipos = STRPOS(files_small[ismall], '_O0')
      small_orbits[ismall]  = STRMID(files_small[ismall], ipos+3, 5)
      small_cameras[ismall] = STRMID(files_small[ismall], ipos+9, 2)
   ENDFOR
   uniq_orbit_ndxs = UNIQ(small_orbits, SORT(small_orbits))
   nuniq = N_ELEMENTS(uniq_orbit_ndxs)
   PRINTF, Unit, 'The following ' + STRTRIM(STRING(nsmall),2) + $
           ' camera files are missing for these ' + nuniq + ' orbits :'
   FOR iuniq=0,nuniq-1 DO BEGIN
      ndxs = WHERE(small_orbits EQ small_orbits[uniq_orbit_ndxs[iuniq]])
      new_small_cameras = STRARR(9)
      new_small_cameras[0:N_ELEMENTS(ndxs)-1] = small_cameras[ndxs]
      PRINTF, Unit, FORMAT='(A,A,9(A,1x))', $
              small_orbits[uniq_orbit_ndxs[iuniq]], ' : ', $
              new_small_cameras
      new_small_cameras = 0
   ENDFOR
ENDIF

END  ;  ProcessL1B2Files

;***************************************************************************
PRO CheckMisrFileOrder, DefaultProjDir
;***************************************************************************
; Construct a listing of all MISR terrain, ellipsoid, geometry, aerosol and
; classifiers products that have been retrieved and another of those that
; have not.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

cameras = STRUPCASE(!KON.Instr.CAM_NAMES)

PathAlias   = 'xPATHx'
OrbitAlias  = 'xORBITx'
PrologAlias = 'xPROLOGx'
CameraAlias = 'xCAMx'

agp_tmplt  = 'MISR_AM1_AGP_PxPATHx_xPROLOGx.hdf'
gmp_tmplt  = 'MISR_AM1_GP_GMP_PxPATHx_O0xORBITx_xPROLOGx.hdf'
clas_tmplt = 'MISR_AM1_TC_CLASSIFIERS_PxPATHx_O0xORBITx_xPROLOGx.hdf'
aero_tmplt = 'MISR_AM1_AS_AEROSOL_PxPATHx_O0xORBITx_xPROLOGx.hdf'
terr_tmplt = 'MISR_AM1_GRP_TERRAIN_GM_PxPATHx_O0xORBITx_xCAMx_xPROLOGx.hdf'
elli_tmplt = 'MISR_AM1_GRP_ELLIPSOID_GM_PxPATHx_O0xORBITx_xCAMx_xPROLOGx.hdf'

;---------------------------------------------------------------------------
; Let the user select the input file containing the list of MISR orbits that
; should be retrieved.
;---------------------------------------------------------------------------

GetLastFilename, 0, !KON.FileTyp.TypeMISRfile, '*.txt', 0, DefaultProjDir, $
                 MisrListFile
IF (MisrListFile EQ '') THEN RETURN

;---------------------------------------------------------------------------
; Get a project name from the filename.
;---------------------------------------------------------------------------

proj_name = ''

npos1 = STRPOS(MisrListFile, '_', /REVERSE_SEARCH)
IF (npos1 LT 0) THEN BEGIN
   npos1 = STRPOS(MisrListFile, !KON.Misc.Slash, /REVERSE_SEARCH)
   IF (npos1 LT 0) THEN npos1 = 0
ENDIF

npos2 = STRPOS(MisrListFile, '.', /REVERSE_SEARCH)
IF (npos2 LT 0) THEN npos2 = STRLEN(MisrListFile) - 1

proj_name = STRMID(MisrListFile, npos1+1, npos2-npos1-1)

;---------------------------------------------------------------------------
; Read the list of MISR ordered orbits that should be retrieved.
;---------------------------------------------------------------------------

buff = ''
OPENR, unit, MisrListFile, /GET_LUN
READF, unit, buff
FREE_LUN, unit

orbit_list = STRSPLIT(buff, ', ', /EXTRACT)
num_orbits = N_ELEMENTS(orbit_list)

;---------------------------------------------------------------------------
; Tell user where the log file will be written.
;---------------------------------------------------------------------------

logfile = DefaultProjDir + !KON.Misc.Slash + 'OrbitOrderStatus_' + $
          proj_name + '.txt'

;---------------------------------------------------------------------------
; Show a dialog box in which the user must select sample files for each of
; the 5 MISR products. The directories are stripped out and the filename
; is used as a template for all other files of the same type.
;---------------------------------------------------------------------------

ReportOnMisrOrder_gui, MisrOrder

ndx0 = STRPOS(MisrOrder[0], '_AGP_')
ndx1 = STRPOS(MisrOrder[1], '_GP_GMP_')
ndx2 = STRPOS(MisrOrder[2], '_TC_CLASSIFIERS_')
ndx3 = STRPOS(MisrOrder[3], '_AS_AEROSOL_')
ndx4 = STRPOS(MisrOrder[4], '_GRP_TERRAIN_')
ndx5 = STRPOS(MisrOrder[5], '_GRP_ELLIPSOID_')

IF (ndx0 LT 0 AND ndx1 LT 0 AND ndx2 LT 0 AND $
    ndx3 LT 0 AND ndx4 LT 0 AND ndx5 LT 0) THEN RETURN

;---------------------------------------------------------------------------
; Specify minimum required size of each file type to be valid.
; NOTE - The test for minimum size has been deactivated.
;---------------------------------------------------------------------------

agp_min_size  = 80000000L
gmp_min_size =  8000000L
clas_min_size = 20000000L
aero_min_size = 15000000L
terr_min_size = 15000000L
elli_min_size = 15000000L

;---------------------------------------------------------------------------
; Open the log file for writing and write the header.
;---------------------------------------------------------------------------

OPENW, unit, logfile, /GET_LUN
PRINTF, unit, 'Project: ' + proj_name

;---------------------------------------------------------------------------
; Set up the templates for the files we want to find before processing each.
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Check on the AGP files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx0 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[0], agp_tmplt, PathAlias, $
                      CameraAlias, agp_dir, new_agp_prolog, agp_template
   ProcessOrbits, unit, orbit_list, num_orbits, agp_template, PathAlias, $
          'XXXX', PrologAlias, new_agp_prolog, agp_min_size, $
          'Order Status for AGP Files for Version ' + new_agp_prolog, $
          '---------------------------------------------'
ENDIF

;---------------------------------------------------------------------------
; Check on the GP_GMP files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx1 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[1], gmp_tmplt, PathAlias, $
                      CameraAlias, gmp_dir, new_gmp_prolog, gmp_template
   ProcessOrbits, unit, orbit_list, num_orbits, gmp_template, PathAlias, $
          OrbitAlias, PrologAlias, new_gmp_prolog, gmp_min_size, $
          'Order Status for GP_GMP Files for Version ' + new_gmp_prolog, $
          '--------------------------------------------------'
ENDIF

;---------------------------------------------------------------------------
; Check on the TC_CLASSIFIERS files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx2 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[2], clas_tmplt, PathAlias, $
                      CameraAlias, clas_dir, new_clas_prolog, clas_template
   ProcessOrbits, unit, orbit_list, num_orbits, clas_template, PathAlias, $
      OrbitAlias, PrologAlias, new_clas_prolog, clas_min_size, $
      'Order Status for TC_CLASSIFIERS Files for Version ' + new_clas_prolog, $
      '----------------------------------------------------------'
ENDIF

;---------------------------------------------------------------------------
; Check on the AS_AEROSOL files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx3 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[3], aero_tmplt, PathAlias, $
                      CameraAlias, aero_dir, new_aero_prolog, aero_template
   ProcessOrbits, unit, orbit_list, num_orbits, aero_template, PathAlias, $
          OrbitAlias, PrologAlias, new_aero_prolog, aero_min_size, $
          'Order Status for AS_AEROSOL Files for Version ' + new_aero_prolog, $
          '------------------------------------------------------'
ENDIF

;---------------------------------------------------------------------------
; Check on the GRP_TERRAIN files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx4 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[4], terr_tmplt, PathAlias, $
                      CameraAlias, terr_dir, new_terr_prolog, terr_template
   ProcessL1B2Files, unit, cameras, orbit_list, num_orbits, terr_template, $
                     PathAlias, OrbitAlias, CameraAlias, PrologAlias, $
                     new_terr_prolog
ENDIF

;---------------------------------------------------------------------------
; Check on the GRP_ELLIPSOID files for listed orbits.
;---------------------------------------------------------------------------

IF (ndx5 GT 0) THEN BEGIN
   GetDirAndTemplate, MisrOrder[5], elli_tmplt, PathAlias, $
                      CameraAlias, elli_dir, new_elli_prolog, elli_template
   ProcessL1B2Files, unit, cameras, orbit_list, num_orbits, elli_template, $
                     PathAlias, OrbitAlias, CameraAlias, PrologAlias, $
                     new_elli_prolog
ENDIF

ndxs                = 0
files_missing       = 0
files_small         = 0
files_valid         = 0
missing_orbits      = 0
missing_cameras     = 0
new_missing_cameras = 0
small_orbits        = 0
small_cameras       = 0
new_small_cameras   = 0

;---------------------------------------------------------------------------
; Close the log file.
;---------------------------------------------------------------------------

closeup:
FREE_LUN, unit
rtrn_val = ChmodCatchError(logfile, '666'O)

mssg = ['The log file reporting the status of ordered files will ' + $
        'be located at: ', logfile]
rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)


END ; CheckMisrFileOrder
