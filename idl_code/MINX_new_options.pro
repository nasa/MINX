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
PRO GetProductFile, OrbitNum, CamName, BlockBeg, BlockEnd, Option, DirName, $
                    FileName, Retval
;***************************************************************************
; function added by D Nelson, 3/05
; Routine gets the names of 1 cloud mask file the user wants to display.
; File name must conform to MISR naming conventions.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

Retval = -1
cam_names = STRUPCASE(!KON.Instr.CAM_NAMES)

;---------------------------------------------------------------------------
; The user selects the cloud mask file to retrieve.  The type depends on the
; passed Option.  The orbit must correcpond to the currently loaded orbit.
; The camera for RCCM must correcpond to the currently loaded camera. Then
; get the directory and file name separately.
;---------------------------------------------------------------------------

orbit_str = STRTRIM(STRING(OrbitNum),2)
nlen = STRLEN(orbit_str)
IF (nlen EQ 5) THEN orbit_str = 'O0' + orbit_str + '*'
IF (nlen EQ 4) THEN orbit_str = 'O00' + orbit_str + '*'
IF (nlen EQ 3) THEN orbit_str = 'O000' + orbit_str + '*'

IF (Option EQ 0) THEN $
   FilenameFilter = ['MISR*RCCM*'+orbit_str+'.hdf']
IF (Option EQ 1 OR Option EQ 2) THEN $
   FilenameFilter = ['MISR*TC_STEREO*'+orbit_str+'.hdf']
IF (Option EQ 3 OR Option EQ 4 OR Option EQ 5) THEN $
   FilenameFilter = ['MISR*TC_CLASSIFIERS*'+orbit_str+'.hdf']
IF (Option EQ 6) THEN $
   FilenameFilter = ['MISR*'+orbit_str+'.hdf']

retry8:
GetLastFilename, 0, !KON.FileTyp.TypeCldMask, FilenameFilter, 0, $
                 file_outpath, input_filename

IF (input_filename EQ '') THEN RETURN

;---------------------------------------------------------------------------
; Determine if the orbit number and camera name are correct. If not, tell
; user to try again.
;---------------------------------------------------------------------------

dir_name = file_outpath
nlen = STRLEN(dir_name)
file_name = STRMID(input_filename, nlen)

npos = STRPOS(file_name, '_O0')
path_num = FIX(STRMID(file_name, npos-3, 3))
orbit_num = LONG(STRMID(file_name, npos+3, 5))
cam_name = STRMID(file_name, npos+9, 2)

IF (orbit_num NE OrbitNum) THEN BEGIN
   mssg = 'You must select a file for orbit ' + $
          STRTRIM(STRING(OrbitNum),2) + '. Try again.'
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   GOTO, retry8
ENDIF

IF (Option EQ 0 AND cam_name NE STRUPCASE(CamName)) THEN BEGIN
   mssg = 'You must select a file for camera ' + $
          CamName + '. Try again.'
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   GOTO, retry8
ENDIF

DirName = dir_name
FileName = file_name

;---------------------------------------------------------------------------
; Find the first and last block number from the file.
;---------------------------------------------------------------------------

GetFirstLastBlocks, input_filename, block_beg, block_end, Retval

IF (block_beg GT BlockBeg OR block_end LT BlockEnd) THEN BEGIN
   mssg = ['This file does not contain all the blocks in the image:', $
           input_filename]
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
   Retval = -1
   RETURN
ENDIF

END ; GetProductFile

;***************************************************************************
PRO GetCamFiles, CallerID, CamFiles, OrbitNum, MISR_or_AirMISR, NCAMS, $
                 whichorbit, CamMask, Retval
;***************************************************************************
; function added by D.Nelson, 2/04; updated 5/05
; Routine gets the names of the 9 camera files the user wants. File names
; must conform to MISR naming conventions and the files must either all be
; in the same directory or in 9 subdirectories with the conventional MISR
; camera names; if in MISR directory structure, camera subdirectories can be
; upper or lower case.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

   Retval = -1
   cam_names = STRUPCASE(!KON.Instr.CAM_NAMES)

   IF (CallerID EQ 1) THEN BEGIN

      ;---------------------------------------------------------------------
      ; The user selects the nadir camera to use. Either ellipsoid or
      ; terrain files will do.
      ;---------------------------------------------------------------------

      IF (!VAR.ProdOrDev EQ !KON.Misc.MINX_PRD_VER) THEN BEGIN
         FilenameFilter = ['MISR*TERRAIN*_AN_*.hdf', $   ; hdf open dialog
                           'MISR*ELLIPSOID*_AN_*.hdf', $ ; shows only these
                           '*.sav']
      ENDIF ELSE BEGIN
         FilenameFilter = ['MISR*TERRAIN*_AN_*.hdf', $   ; hdf open dialog
                           'MISR*ELLIPSOID*_AN_*.hdf', $ ; shows only these
                           'AIRMISR*_AN_*.hdf', $
                           '*.sav']
      ENDELSE

      GetLastFilename, 0, !KON.FileTyp.TypeL1B2, FilenameFilter, 1, $
                       file_outpath, input_filename

      IF (input_filename EQ '') THEN RETURN

   ENDIF ELSE BEGIN

      input_filename = CamFiles[0]
      npos = STRPOS(input_filename, !KON.Misc.Slash, /REVERSE_SEARCH)
      file_outpath = STRMID(input_filename, 0, npos+1)

   ENDELSE

   ;------------------------------------------------------------------------
   ; Set the flag for MISR, AirMISR or Restore.
   ;------------------------------------------------------------------------

   split = STRSPLIT(input_filename,'.',/EXTRACT)
   extension = split[N_ELEMENTS(split)-1]

   CASE 1 OF

     extension EQ 'sav' : BEGIN

       ;--------------------------------------------------------------------
       ; Restore session.
       ;--------------------------------------------------------------------

       MISR_or_AirMISR = -1    ; Restore session flag.
       CamFiles = input_filename ; Name of the save file.
       Retval = 0
       RETURN                  ; exit this subroutine.
       END

     STRUPCASE(STRMID(FILE_BASENAME(input_filename),0,4)) EQ 'MISR' : BEGIN

       MISR_or_AirMISR = 0     ; MISR flag.
       END

     STRUPCASE(STRMID(FILE_BASENAME(input_filename),0,4)) EQ 'AIRM' : BEGIN

       MISR_or_AirMISR = 1     ; AirMISR flag.
       END

   ENDCASE

   IF MISR_or_AirMISR EQ 0 THEN BEGIN

     ;----------------------------------------------------------------------
     ; Determine whether or not the nadir camera directory includes a camera
     ; abbreviation component for any camera and get the orbit number.
     ;----------------------------------------------------------------------

     npos = STRLEN(file_outpath)
     IF (STRMID(file_outpath,npos-1) EQ !KON.Misc.Slash) THEN $
        npos = npos - 1

     dir = 1
     IF (STRMID(file_outpath, npos-2, 2) EQ 'AN') THEN BEGIN
        dir = 2
        file_outpath = STRMID(file_outpath, 0, npos-2)
     ENDIF   

   ENDIF

   input_filename = FILE_BASENAME(input_filename)
   input_filename = (str_sep(input_filename,path_sep())) $
                   [n_elements(str_sep(input_filename,path_sep()))-1]

   FOR icam = 0, NCAMS-1 DO BEGIN
      cam_comp = '_' + cam_names[icam] + '_'
      npos_an = STRPOS(input_filename, cam_comp, /REVERSE_SEARCH) + 1
      IF (npos_an GT 0) THEN BREAK
   ENDFOR

   IF (npos_an EQ 0) THEN BEGIN
      mssg = 'Unable to find a valid MISR file name.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   npos = STRPOS(input_filename, '_O0')
   OrbitNum[whichorbit] = LONG(STRMID(input_filename, npos+3, 5))

   ;------------------------------------------------------------------------
   ; Search for the other files in their directories or in same directory
   ; as the An camera file. We must have all 9 cameras.
   ;------------------------------------------------------------------------

   IF (N_ELEMENTS(CamFiles) EQ 1) THEN CamFiles = STRARR(18)

   numbad = 0

   CASE 1 OF
      MISR_or_AirMISR EQ 0 : BEGIN     ;  MISR

         FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
            IF (CamMask[icam] EQ 0) THEN CONTINUE
            camdir = ''
            IF (dir EQ 2) THEN camdir = cam_names[icam] + !KON.Misc.Slash 
            CamFiles[icam+9*whichorbit] = file_outpath + camdir

            temp_str = input_filename
            STRPUT, temp_str, cam_names[icam], npos_an
            CamFiles[icam+9*whichorbit] = CamFiles[icam+9*whichorbit] + $
                                          temp_str

            ;---------------------------------------------------------------
            ; Check for existence and resolve wildcards in the name.
            ;---------------------------------------------------------------

            filnam = FILE_SEARCH(CamFiles[icam+9*whichorbit])
            IF (filnam EQ '') THEN BEGIN
               mssg = ["Missing file:", CamFiles[icam+9*whichorbit]]
               rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
               numbad = numbad + 1
            ENDIF ELSE BEGIN
               CamFiles[icam+9*whichorbit] = filnam
            ENDELSE
         ENDFOR

         !VAR.CurrFiles.NadirFile = CamFiles[!KON.Instr.An]
      END

      MISR_or_AirMISR EQ 1 : BEGIN     ;  AirMISR

         FOR icam = 0,!KON.Instr.NCAM-1 DO BEGIN
            CamFiles[icam+9*whichorbit] = list[icam]

            ;---------------------------------------------------------------
            ; Check for existence and resolve wildcards in the name.
            ;---------------------------------------------------------------

            filnam = FILE_SEARCH(CamFiles[icam+9*whichorbit])
            IF (filnam EQ '') THEN BEGIN
               mssg = ["Missing file:", CamFiles[icam+9*whichorbit]]
               rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
               numbad = numbad + 1
            ENDIF ELSE BEGIN
               CamFiles[icam+9*whichorbit] = filnam
            ENDELSE
         ENDFOR

      END
   ENDCASE

   IF (numbad NE 0) THEN BEGIN
      mssg = ['One or more camera files is missing.', $
              'Fix the problem and try again.']
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   Retval = 0

END ; GetCamFiles

;***************************************************************************
PRO GetOrbitParams_part1, CamFiles, CoordStruct, whichorbit, BlockULCx, $
                          BlockULCy, BlockLRCx, BlockLRCy, path_number, Retval
;***************************************************************************
; function added by D.Nelson, 2/04
; Retrieve the orbit parameters from the nadir camera L1B2 product. This
; routine gets called BEFORE the user has specified which blocks.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1
   path_number = -1
   
   sd_id = HDF_StartInterface(CamFiles[4+9*whichorbit])
   IF (sd_id LE 0) THEN RETURN

   dindex = HDF_SD_ATTRFIND(sd_id, 'Path_number')
   IF (dindex LE 0) THEN BEGIN
      mssg = ['Could not find Path_number in:', CamFiles[4+9*whichorbit]]
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      HDF_SD_END, sd_id
      RETURN
   ENDIF
   HDF_SD_ATTRINFO, sd_id, dindex, DATA=path_number

   dindex = HDF_SD_ATTRFIND(sd_id, 'SOM_parameters.som_orbit.i')
   IF (dindex LE 0) THEN BEGIN
      mssg = ['Could not find som_orbit.i in:', CamFiles[4+9*whichorbit]]
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      HDF_SD_END, sd_id
      RETURN
   ENDIF
   HDF_SD_ATTRINFO, sd_id, dindex, DATA=orbit_inclination

   dindex = HDF_SD_ATTRFIND(sd_id, 'SOM_parameters.som_orbit.P2P1')
   IF (dindex LE 0) THEN BEGIN
      mssg = ['Could not find som_orbit.P2P1 in:', CamFiles[4+9*whichorbit]]
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      HDF_SD_END, sd_id
      RETURN
   ENDIF
   HDF_SD_ATTRINFO, sd_id, dindex, DATA=orbit_period

   dindex = HDF_SD_ATTRFIND(sd_id, 'SOM_parameters.som_orbit.lambda0')
   IF (dindex LE 0) THEN BEGIN
      mssg = ['Could not find som_orbit.lambda0 in:', CamFiles[4+9*whichorbit]]
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      HDF_SD_END, sd_id
      RETURN
   ENDIF
   HDF_SD_ATTRINFO, sd_id, dindex, DATA=orbit_longitude

   HDF_SD_END, sd_id

   GetFirstLastBlocks, CamFiles[4+9*whichorbit], start_block, end_block, status

   ;------------------------------------------------------------------------
   ; Get the block corner coords from the HDF file.
   ;------------------------------------------------------------------------

   fileid = HDF_OPEN(CamFiles[4+9*whichorbit], /READ)
   vd_ref = HDF_VD_FIND(fileid, 'PerBlockMetadataCommon')
   vdata  = HDF_VD_ATTACH(fileid, vd_ref)
   nrec   = HDF_VD_READ(vdata, BlockULCx, FIELDS='Block_coor_ulc_som_meter.x')
   nrec   = HDF_VD_READ(vdata, BlockULCy, FIELDS='Block_coor_ulc_som_meter.y')
   nrec   = HDF_VD_READ(vdata, BlockLRCx, FIELDS='Block_coor_lrc_som_meter.x')
   nrec   = HDF_VD_READ(vdata, BlockLRCy, FIELDS='Block_coor_lrc_som_meter.y')
   HDF_VD_DETACH, vdata
   HDF_CLOSE, fileid

   CATCH, iErr
   IF (iErr NE 0) THEN BEGIN
      IF (CamFiles[4+9*whichorbit] EQ '') THEN BEGIN
         mssg = ['There are no camera files specified for the 2nd orbit.', $
                 'Try again by clicking the "Select Orbit 2 Nadir HDF File"', $
                 'button and selecting a camera file.']
      ENDIF ELSE BEGIN
         mssg = ['There is a problem with the file named:', $
                 CamFiles[4+9*whichorbit], 'Reorder the file or ' + $
                 'otherwise fix the problem and try running again.']
      ENDELSE
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      CATCH, /CANCEL  
      RETURN  
   ENDIF

   BlockULCx = REFORM(BlockULCx) / 1000.0
   BlockULCy = REFORM(BlockULCy) / 1000.0
   BlockLRCx = REFORM(BlockLRCx) / 1000.0
   BlockLRCy = REFORM(BlockLRCy) / 1000.0

   ;------------------------------------------------------------------------
   ; Copy the scalar orbit parameters from the read structure into the
   ; coordinate structure.  Ask the user what block range to use and just
   ; copy those data into the structure.
   ;------------------------------------------------------------------------

   CoordStruct.(whichorbit).PathNum  = path_number
   CoordStruct.(whichorbit).OrbitInc = orbit_inclination
   CoordStruct.(whichorbit).OrbitPer = orbit_period * 60.0 * 24.0
   CoordStruct.(whichorbit).OrbitLon = orbit_longitude

  ;--------------------------------------------------------------------------
  ; Save the map projection parameters for this orbit.
  ;--------------------------------------------------------------------------

   map_parms = MAP_PROJ_INIT(122, $        ; 'Space Oblique Mercator A'
                             DATUM=8, $    ; 'GRS 1980/WGS 84'
                             SOM_INCLINATION=orbit_inclination, $
                             SOM_LONGITUDE=orbit_longitude, $
                             SOM_PERIOD=orbit_period * 60.0 * 24.0, $
                             /RADIANS)

   CoordStruct.(whichorbit).ProjParms = map_parms

   ;------------------------------------------------------------------------
   ; Copy block range to use into coordinate structure.
   ;------------------------------------------------------------------------

   block_beg = start_block
   block_end = end_block

   CoordStruct.(whichorbit).BlkBeg = block_beg
   CoordStruct.(whichorbit).BlkEnd = block_end
   CoordStruct.(whichorbit).NumBlk = block_end - block_beg + 1

   ;------------------------------------------------------------------------
   ; Extract the orbit acquisition date from the core metadata.
   ;------------------------------------------------------------------------

   sd_id = HDF_StartInterface(CamFiles[4+9*whichorbit])
   IF (sd_id LE 0) THEN RETURN

   sdndx = HDF_SD_ATTRFIND(sd_id, 'coremetadata')
   HDF_SD_ATTRINFO, sd_id, sdndx, DATA=coremetadata
   HDF_SD_END, sd_id

   npos = STRPOS(coremetadata, 'EQUATORCROSSINGDATE')
   npos = STRPOS(coremetadata, '"2', npos)
   CoordStruct.(whichorbit).OrbitDate = STRMID(coremetadata, npos+1,10)

   CATCH, /CANCEL  

   Retval = 0

END ; GetOrbitParams_part1

;***************************************************************************
PRO GetOrbitParams_part2, CoordStruct, Retval, whichorbit, BlockULCx, $
                          BlockULCy, BlockLRCx, BlockLRCy
;***************************************************************************
; This routine gets called AFTER the user has specified which blocks.
; NOTE - the returned coordinates represent the outer EDGES of the outer
; pixels rather than the CENTER coordinates of the outer pixels.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Create arrays and copy the vector parameters from the read structure
   ; into the coordinate structure.
   ;------------------------------------------------------------------------

   CoordStruct.(whichorbit).ULCalongSOM = PTR_NEW( $
                    BlockULCx[CoordStruct.(whichorbit).BlkBeg-1: $
                              CoordStruct.(whichorbit).BlkEnd-1], $
                              /NO_COPY )
   CoordStruct.(whichorbit).ULCcrossSOM = PTR_NEW( $
                    BlockULCy[CoordStruct.(whichorbit).BlkBeg-1: $
                              CoordStruct.(whichorbit).BlkEnd-1], $
                              /NO_COPY )
   CoordStruct.(whichorbit).LRCalongSOM = PTR_NEW( $
                    BlockLRCx[CoordStruct.(whichorbit).BlkBeg-1: $
                              CoordStruct.(whichorbit).BlkEnd-1], $
                              /NO_COPY )
   CoordStruct.(whichorbit).LRCcrossSOM = PTR_NEW( $
                    BlockLRCy[CoordStruct.(whichorbit).BlkBeg-1: $
                              CoordStruct.(whichorbit).BlkEnd-1], $
                              /NO_COPY )
   Retval = 0

END ; GetOrbitParams_part2

;***************************************************************************
PRO WndwCrdToMisrCrd, Curframe, WndwCoord, MisrCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04.
; Procedure updated by D.Nelson, 9/12 for speed w/ multiple points.
; Procedure converts window pixel coordinates to MISR SOM block/across/along
; coordinates.
; MisrCoord values are output as integers representing 275 m pixels.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(WndwCoord) / 2
   MisrCoord = INTARR(3, NumPts)

   ;------------------------------------------------------------------------
   ; Isolate x and y coords, count the points and find bad values.
   ;------------------------------------------------------------------------

   WndwX = REFORM(WndwCoord[0,*])
   WndwY = REFORM(WndwCoord[1,*])

   badndxs = WHERE(WndwX LT 0 OR WndwX GE CoordStruct.(whichorbit).LRCwndwX OR $
                   WndwY LT 0 OR WndwY GE CoordStruct.(whichorbit).ULCwndwY, $
                   numndxs)

   ;------------------------------------------------------------------------
   ; Convert window coords to MISR coords (block/line/samp).
   ;------------------------------------------------------------------------

   wndw_max = CoordStruct.(whichorbit).NumBlk * $
              CoordStruct.(whichorbit).DimAlong - 1

   block = FLOOR((wndw_max - WndwY) / (CoordStruct.(whichorbit).DimAlong)) + 1

   MisrSamp = WndwX - (*CoordStruct.(whichorbit).BlkPixOffset)[block-1,1]
   MisrLine = (CoordStruct.(whichorbit).NumBlk - block + 1) * $
               CoordStruct.(whichorbit).DimAlong - WndwY - 1
   MisrBlock = CoordStruct.(whichorbit).BlkBeg + block - 1

   IF (numndxs GT 0) THEN BEGIN
      MisrSamp[badndxs]  = FIX(!KON.Misc.BADVALUE_REAL)
      MisrLine[badndxs]  = FIX(!KON.Misc.BADVALUE_REAL)
      MisrBlock[badndxs] = FIX(!KON.Misc.BADVALUE_REAL)
   ENDIF

   MISRcoord[0,*] = MisrSamp
   MISRcoord[1,*] = MisrLine
   MISRcoord[2,*] = MisrBlock

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   WndwX = 0
   WndwY = 0
   badndxs = 0
   block = 0
   MisrSamp = 0
   MisrLine = 0
   MisrBlock = 0
   Retval = 0

END  ;  WndwCrdToMisrCrd

;***************************************************************************
PRO MisrCrdToWndwCrd, Curframe, MisrCoord, WndwCoord, InvertY, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04. Procedure converts MISR SOM
; block/across/along coordinates to window pixel coords. WndwCoord values
; are output as integers representing 275 m pixels.
; If OutOfSwath = 0, procedure returns BADVALUE_REAL for points outside the
; limits of MISR blocks. If 1, "valid" results are returned.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(MisrCoord) / 3
   WndwCoord = INTARR(2, NumPts)

   FOR ipts = 0L,NumPts-1 DO BEGIN

      WndwCoord[*,ipts] = FIX(!KON.Misc.BADVALUE_REAL)

      IF (MisrCoord[2,ipts] NE FIX(!KON.Misc.BADVALUE_REAL)) THEN BEGIN
         block = MisrCoord[2,ipts] - CoordStruct.(whichorbit).BlkBeg + 1

         IF (block GE 1 AND block LE CoordStruct.(whichorbit).NumBlk AND $
             MisrCoord[0,ipts] GE 0 AND MisrCoord[0,ipts] LT $
                                   !KON.Instr.HI_RES_PIX_CROSS AND $
             MisrCoord[1,ipts] GE 0 AND MisrCoord[1,ipts] LT $
                                   !KON.Instr.HI_RES_PIX_ALONG) THEN BEGIN
                        
            WndwCoord[0,ipts] = MisrCoord[0,ipts] + $
                         (*CoordStruct.(whichorbit).BlkPixOffset)[block-1, 1]
            WndwCoord[1,ipts] = InvertY $
                        ? (CoordStruct.(whichorbit).NumBlk - block + 1) * $
                           CoordStruct.(whichorbit).DimAlong - $
                           MisrCoord[1, ipts] - 1 $
                        : (block - 1) * CoordStruct.(whichorbit).DimAlong + $
                           MisrCoord[1,ipts]
         ENDIF
      ENDIF
   ENDFOR

   Retval = 0

END  ;  MisrCrdToWndwCrd

;***************************************************************************
PRO MisrCrdToSomCrd, Curframe, MisrCoord, SomCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04.
; Procedure updated by D.Nelson, 9/12 for speed w/ multiple points.
; Procedure converts MISR SOM block/across/along coordinates to SOM across/
; along meters from start of swath.
; SOM coords are output in DOUBLE km.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(MisrCoord) / 3
   SomCoord = DBLARR(2, NumPts)

   ;------------------------------------------------------------------------
   ; Find SOM coordinates in meters at center of ULC corner pixel of entire
   ; display area.
   ;------------------------------------------------------------------------

   indx = CoordStruct.(whichorbit).NumBlk-1
   ULCcrossc = ((*CoordStruct.(whichorbit).ULCcrossSOM)[indx] < $
                (*CoordStruct.(whichorbit).ULCcrossSOM)[0]) + $
               (!KON.Instr.HI_RES_PIX_SIZE / 2.0)
   ULCalongc = (*CoordStruct.(whichorbit).ULCalongSOM)[0] + $
               (!KON.Instr.HI_RES_PIX_SIZE / 2.0)

   ;------------------------------------------------------------------------
   ; Convert points from MISR block/across/along to SOM meters.
   ;------------------------------------------------------------------------

   samp  = FLOAT(REFORM(MisrCoord[0,*]))
   line  = FLOAT(REFORM(MisrCoord[1,*]))
   block = FLOAT(REFORM(MisrCoord[2,*])) - CoordStruct.(whichorbit).BlkBeg + 1.0

   SOMcrosspix = samp + (*CoordStruct.(whichorbit).BlkPixOffset)[block-1,1]
   SOMalongpix = line + ((block - 1) * CoordStruct.(whichorbit).DimAlong)

   SomCoord[0,*] = SOMcrosspix * !KON.Instr.HI_RES_PIX_SIZE + ULCcrossc
   SomCoord[1,*] = SOMalongpix * !KON.Instr.HI_RES_PIX_SIZE + ULCalongc

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   samp  = 0
   line  = 0
   block = 0
   SOMcrosspix = 0
   SOMalongpix = 0

   Retval = 0

END  ;  MisrCrdToSomCrd

;***************************************************************************
PRO SomCrdToMisrCrd, Curframe, OutOfSwath, SomCoord, MisrCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04.
; procedure converts SOM across/along km from start of swath to MISR SOM
; block/across/along coordinates (using 275 m pixels).
; If OutOfSwath = 0, procedure returns BADVALUE_REAL for points outside the
; limits of MISR blocks. If 1, "valid" results are returned.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(SomCoord) / 2
   MisrCoord = INTARR(3, NumPts)

   ;------------------------------------------------------------------------
   ; Find SOM coordinates in km at center of ULC corner pixel of entire
   ; display area.
   ;------------------------------------------------------------------------

   indx = CoordStruct.(whichorbit).NumBlk - 1
   ULCcrossc = ((*CoordStruct.(whichorbit).ULCcrossSOM)[indx] < $
                (*CoordStruct.(whichorbit).ULCcrossSOM)[0]) + $
               (!KON.Instr.HI_RES_PIX_SIZE / 2.0)
   ULCalongc = (*CoordStruct.(whichorbit).ULCalongSOM)[0] + $
               (!KON.Instr.HI_RES_PIX_SIZE / 2.0)

   ;------------------------------------------------------------------------
   ; For each point convert from SOM km to MISR across/along.
   ;------------------------------------------------------------------------

   FOR ipts=0L,NumPts-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Convert SOM km to MISR SOM pixels relative to ULC corner of display
      ; area. These may be negative.
      ;---------------------------------------------------------------------
   
      SOMcrosspix = (SomCoord[0,ipts] - ULCcrossc) / !KON.Instr.HI_RES_PIX_SIZE
      SOMalongpix = (SomCoord[1,ipts] - ULCalongc) / !KON.Instr.HI_RES_PIX_SIZE

      ;---------------------------------------------------------------------
      ; Determine the block, line and sample numbers. The block number is
      ; 1-based and is stored as the actual MISR block number.
      ;---------------------------------------------------------------------

      block = FLOOR((SOMalongpix + 0.5) / CoordStruct.(whichorbit).DimAlong) + 1

      IF (block LT 1 OR block GT CoordStruct.(whichorbit).NumBlk) THEN BEGIN
         IF (OutOfSwath EQ 1) THEN BEGIN
            line = (block LT 1) ? -5 : (CoordStruct.(whichorbit).DimAlong +5)
            blk_num = (block LT 1) ? 1 : CoordStruct.(whichorbit).NumBlk
            samp = ROUND(SOMcrosspix - $
                         (*CoordStruct.(whichorbit).BlkPixOffset)[blk_num-1,1])
         ENDIF ELSE BEGIN
            line  = FIX(!KON.Misc.BADVALUE_REAL)
            samp  = FIX(!KON.Misc.BADVALUE_REAL)
         ENDELSE
         
      ENDIF ELSE BEGIN
         line = ROUND(SOMalongpix - $
                      ((block - 1) * CoordStruct.(whichorbit).DimAlong))
         samp = ROUND(SOMcrosspix - $
                      (*CoordStruct.(whichorbit).BlkPixOffset)[block-1,1])

         IF (OutOfSwath EQ 0 AND $
             (line LT 0 OR line GT !KON.Instr.HI_RES_PIX_ALONG OR $
              samp LT 0 OR samp GT !KON.Instr.HI_RES_PIX_CROSS)) THEN BEGIN
            block = FIX(!KON.Misc.BADVALUE_REAL)
            line  = FIX(!KON.Misc.BADVALUE_REAL)
            samp  = FIX(!KON.Misc.BADVALUE_REAL)
         ENDIF
      ENDELSE

      MisrCoord[0,ipts] = samp
      MisrCoord[1,ipts] = line
      IF (block NE FIX(!KON.Misc.BADVALUE_REAL)) THEN $
         block = block + CoordStruct.(whichorbit).BlkBeg - 1
      MisrCoord[2,ipts] = block

   ENDFOR

   Retval = 0

END  ;  SomCrdToMisrCrd

;***************************************************************************
PRO SomCrdToMisrCrd_NoOrbit, SomCoord, MisrCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 9/12.
; Procedure converts SOM across/along meters from start of swath to MISR SOM
; block/across/along coordinates. Used when no orbit data are loaded, so must
; make some assumptions.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   NumPts = N_ELEMENTS(SomCoord) / 2
   MisrCoord = INTARR(3, NumPts)

   ;------------------------------------------------------------------------
   ; Set size of pixels in meters.
   ;------------------------------------------------------------------------

   PixSize = !KON.Instr.HI_RES_PIX_SIZE

   ;------------------------------------------------------------------------
   ; Set SOM coordinates in km at center of ULC corner pixel of entire swath.
   ;------------------------------------------------------------------------

   ULCcrossc = !KON.Instr.ULC_SOM_CROSS + (PixSize / 2.0)
   ULCalongc = !KON.Instr.ULC_SOM_ALONG + (PixSize / 2.0)

   ;------------------------------------------------------------------------
   ; For each point convert from SOM km to MISR across/along.
   ;------------------------------------------------------------------------

   FOR ipts=0L,NumPts-1 DO BEGIN

      ;---------------------------------------------------------------------
      ; Convert SOM km to MISR SOM pixels relative to ULC corner of display
      ; area. These may be negative.
      ;---------------------------------------------------------------------
   
      SOMcrosspix = (SomCoord[0,ipts] - ULCcrossc) / PixSize
      SOMalongpix = (SomCoord[1,ipts] - ULCalongc) / PixSize

      ;---------------------------------------------------------------------
      ; Determine the block, line and sample numbers. The block number is
      ; 1-based and is stored as the actual MISR block #.
      ;---------------------------------------------------------------------

      block = FLOOR((SOMalongpix + 0.5) / !KON.Instr.HI_RES_PIX_ALONG) + 1

      IF (block LT 1 OR block GT !KON.Instr.NUM_BLOCKS) THEN BEGIN
         block = FIX(!KON.Misc.BADVALUE_REAL)
         line  = FIX(!KON.Misc.BADVALUE_REAL)
         samp  = FIX(!KON.Misc.BADVALUE_REAL)
      ENDIF ELSE BEGIN
         line = ROUND(SOMalongpix - ((block-1) * !KON.Instr.HI_RES_PIX_ALONG))
         rel_offset = 0L
         FOR iblk=1,block-1 DO BEGIN
            rel_offset += !KON.Instr.RelOffset[iblk]
         ENDFOR
         samp = ROUND(SOMcrosspix - rel_offset)

         IF (line LT 0 OR line GT !KON.Instr.HI_RES_PIX_ALONG OR $
             samp LT 0 OR samp GT !KON.Instr.HI_RES_PIX_CROSS) THEN BEGIN
            block = FIX(!KON.Misc.BADVALUE_REAL)
            line  = FIX(!KON.Misc.BADVALUE_REAL)
            samp  = FIX(!KON.Misc.BADVALUE_REAL)
         ENDIF
      ENDELSE

      MisrCoord[0,ipts] = samp
      MisrCoord[1,ipts] = line
      MisrCoord[2,ipts] = block

   ENDFOR

   Retval = 0

END  ;  SomCrdToMisrCrd_NoOrbit

;***************************************************************************
PRO LatLonToBLS, Path, Lat, Lon, Block, Line, Samp
;***************************************************************************
; Compute MISR block/line/sample from lat/lon given a MISR path. Standard
; projection parameters must be used rather than those in the metadata of
; an HDF file. There's no HDF file available here.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

LonlatCoord = DOUBLE([[Lon, Lat]])

LonlatCrdToSomCrd, 1, Path, LonlatCoord, SomCoord, Retval
SomCrdToMisrCrd_NoOrbit, SomCoord, MisrCoord, Retval

Samp  = MisrCoord[0]
Line  = MisrCoord[1]
Block = MisrCoord[2]

END  ;  LatLonToBLS

;***************************************************************************
PRO SomCrdToLonlatCrd, Curframe, SomCoord, LonlatCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04.
; Procedure updated by D.Nelson, 9/12 for speed w/ multiple points.
; Procedure converts SOM across/along meters from start of swath into lon/lat
; coordinates on the WGS 84 spheroid.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(SomCoord) / 2
   LonlatCoord = DBLARR(2, NumPts)

   ;------------------------------------------------------------------------
   ; Use projection parameters to convert the coordinates to lon/lat. Input
   ; Som coords are in km and use x as the vertical axis - change for MINX.
   ;------------------------------------------------------------------------

   som_coords = SomCoord
   som_coords[0,*] = SomCoord[1,*] * 1000.0
   som_coords[1,*] = SomCoord[0,*] * 1000.0

   LonlatCoord = MAP_PROJ_INVERSE(som_coords, $
                          MAP_STRUCTURE=CoordStruct.(whichorbit).ProjParms)

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   som_coords = 0
   Retval = 0

END  ;  SomCrdToLonlatCrd

;***************************************************************************
PRO LonlatCrdToSomCrd, Curframe, Path, LonlatCoord, SomCoord, Retval
;***************************************************************************
; Procedure added by D.Nelson, 2/04.
; Procedure converts lon/lat coordinates on the WGS 84 spheroid to SOM
; across/along km from start of swath. If Path is non-zero, use standard
; projection parameters rather than those from a MISR product file.
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   whichorbit = (Curframe GT 9)
   NumPts = N_ELEMENTS(LonlatCoord) / 2
   SomCoord = DBLARR(2, NumPts)

   IF (Path EQ 0) THEN BEGIN
      map_parms = CoordStruct.(whichorbit).ProjParms
   ENDIF ELSE BEGIN
      OrbitInc = !KON.Instr.SOM_INCLIN * !DPI / 180.0
      OrbitPer = !KON.Instr.SOM_PERIOD
      OrbitLon = (!KON.Instr.SOM_LONG_PATH1 + $
                  (Path - 1) * !KON.Instr.DEG_OFFSET) * !DPI / 180.0

      map_parms = MAP_PROJ_INIT(122, $     ; 'Space Oblique Mercator A'
                                DATUM=8, $ ; 'GRS 1980/WGS 84'
                                SOM_INCLINATION=OrbitInc, $
                                SOM_LONGITUDE=OrbitLon, $
                                SOM_PERIOD=OrbitPer, /RADIANS)
   ENDELSE

   ;------------------------------------------------------------------------
   ; Use the projection parameters to convert the coordinates to SOM.
   ;------------------------------------------------------------------------

   som_coords = MAP_PROJ_FORWARD(LonlatCoord, MAP_STRUCTURE=map_parms)

   ;------------------------------------------------------------------------
   ; Som coords use x as the vertical axis - change nomenclature for MINX.
   ;------------------------------------------------------------------------

   SomCoord = som_coords
   SomCoord[0,*] = som_coords[1,*] / 1000.0
   SomCoord[1,*] = som_coords[0,*] / 1000.0

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   som_coords = 0
   map_parms = 0
   Retval = 0

END  ;  LonlatCrdToSomCrd

;***************************************************************************
PRO ShowCoordinates, State, Xpix, Ypix
;***************************************************************************

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Show the window's pixel coordinates.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesCoordWndwText, SET_VALUE = $
                   STRTRIM(STRING(Xpix),2) + ' / ' + STRTRIM(STRING(ypix),2)

   whichorbit = (State.curFrame GT 9)

   CASE 1 OF

      CoordStruct.(whichorbit).OrbitNum GT -1L : BEGIN		; MISR

         ;------------------------------------------------------------------
         ; Show the window's MISR SOM coordinates.
         ;------------------------------------------------------------------

         WndwCrdToMisrCrd, State.Curframe, [[Xpix,Ypix]], misr_crds, retval

         IF (misr_crds[0] NE FIX(!KON.Misc.BADVALUE_REAL)) THEN BEGIN

            WIDGET_CONTROL, State.wFramesCoordSomText, SET_VALUE = $
                            STRTRIM(STRING(misr_crds[2,0]),2) + ' / ' + $
                            STRTRIM(STRING(misr_crds[0,0]),2) + ' / ' + $
                            STRTRIM(STRING(misr_crds[1,0]),2)

            ;---------------------------------------------------------------
            ; Show the window's Lat/Lon coordinates.
            ;---------------------------------------------------------------

            MisrCrdToSomCrd,   State.Curframe, misr_crds, som_crds, retval
            SomCrdToLonlatCrd, State.Curframe, som_crds, lonlat_crds, retval

            WIDGET_CONTROL, State.wFramesCoordGeoText, SET_VALUE = $
                   STRTRIM(STRING(FORMAT='(F8.3)',lonlat_crds[0,0]),2) + ' / ' + $
                   STRTRIM(STRING(FORMAT='(F7.3)',lonlat_crds[1,0]),2)
         ENDIF
      END

      CoordStruct.(whichorbit).OrbitNum EQ -1L : BEGIN   ; AirMISR
      END

   ENDCASE

   ;------------------------------------------------------------------------
   ; Show the window's data values.
   ;------------------------------------------------------------------------

   IF (State.curframe GT !VAR.WORK_WNDW) THEN BEGIN
      numbnd = CoordStruct.(whichorbit).num_band
      IF (numbnd EQ 1) THEN BEGIN
         pixel_val = GetRawImage(Xpix, Xpix, State.sizey-Ypix-1, $
                                 State.sizey-Ypix-1, 0, State.curframe-1, $
                                 !KON.Instr.HI_RES_PIX_SIZE, $
                                 !KON.Misc.INTERP_TYPE_SAMP)

         pixel_str = (pixel_val GT 0.0) ? $
                            STRING(FORMAT='(F6.4)',pixel_val) : ' NA '
      ENDIF ELSE BEGIN
         pixel_val = [0.0, 0.0, 0.0, 0.0]
         FOR ibnd=0,3 DO BEGIN
            pixel_val[ibnd] = $
                  GetRawImage(Xpix, Xpix, State.sizey-Ypix-1, $
                              State.sizey-Ypix-1, ibnd, State.curframe-1, $
                              !KON.Instr.HI_RES_PIX_SIZE, $
                              !KON.Misc.INTERP_TYPE_SAMP)
         ENDFOR

         pixel_str0 = (pixel_val[0] GT 0.0) ? $
                              STRING(FORMAT='(F5.3)',pixel_val[0]) : ' NA '
         pixel_str1 = (pixel_val[1] GT 0.0) ? $
                              STRING(FORMAT='(F5.3)',pixel_val[1]) : ' NA '
         pixel_str2 = (pixel_val[2] GT 0.0) ? $
                              STRING(FORMAT='(F5.3)',pixel_val[2]) : ' NA '
         pixel_str3 = (pixel_val[3] GT 0.0) ? $
                              STRING(FORMAT='(F5.3)',pixel_val[3]) : ' NA '
         pixel_str = pixel_str0 + '/' + pixel_str1 + '/' + $
                     pixel_str2 + '/' + pixel_str3
      ENDELSE
   ENDIF ELSE BEGIN
      pixel_val = WorkImage[Xpix,State.sizey-Ypix-1,*]

      pixel_str0 = (pixel_val[0] GT 0.0) ? $
                           STRING(FORMAT='(F5.3)',pixel_val[0]) : ' NA '
      pixel_str1 = (pixel_val[1] GT 0.0) ? $
                           STRING(FORMAT='(F5.3)',pixel_val[1]) : ' NA '
      pixel_str2 = (pixel_val[2] GT 0.0) ? $
                           STRING(FORMAT='(F5.3)',pixel_val[2]) : ' NA '
      pixel_str = pixel_str0 + '/' + pixel_str1 + '/' + pixel_str2
   ENDELSE

   pixel_val = 0

   WIDGET_CONTROL, State.wFramesCoordDataText, SET_VALUE = pixel_str

   !VAR.WndwCoord.LLPickCrds = lonlat_crds
   !VAR.WndwCoord.SomPickCrds = som_crds
   !VAR.WndwCoord.MisrPickCrds = misr_crds

END  ;  ShowCoordinates

;***************************************************************************
PRO MISRVision_eh, event
;***************************************************************************

COMMON misrvision, MISRVis_rgborb, MISRVis_rgbcam, MISRVis_rgbbnd

COMPILE_OPT IDL2, LOGICAL_PREDICATE

WIDGET_CONTROL, event.top, GET_UVALUE=widget_struct, /NO_COPY

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   MISRVis_rgborb = -1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF

;---------------------------------------------------------------------------
; This is just a convenient place to put the code to determine the maximum
; amount of memory has been used at once.
;---------------------------------------------------------------------------

SHOW_MEMORY = 0

IF (SHOW_MEMORY) THEN BEGIN
   maxmem = MEMORY(/HIGHWATER)
   mssg = 'Highwater mark on memory usage (bytes) = ' + $
          STRTRIM(STRING(maxmem),2)
   rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
ENDIF

CASE 1 OF

   (event.id EQ widget_struct.orbit_btn1) : BEGIN
   END

   (event.id EQ widget_struct.orbit_btn2) : BEGIN
   END

   ;------------------------------------------------------------------------
   ; OK, accept new values.  Actually, the values are already  accepted, so
   ; just kill the gui.
   ;------------------------------------------------------------------------

   (event.id EQ widget_struct.ok_button) : BEGIN
      btn_val = WIDGET_INFO(widget_struct.orbit_btn1, /BUTTON_SET)
      IF (btn_val EQ 1) THEN MISRVis_rgborb = 0
      btn_val = WIDGET_INFO(widget_struct.orbit_btn2, /BUTTON_SET)
      IF (btn_val EQ 1) THEN MISRVis_rgborb = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   ;------------------------------------------------------------------------
   ; Cancel.  Set the cancel flag and kill the gui.
   ;------------------------------------------------------------------------

   (event.id EQ widget_struct.cancel_button) : BEGIN
      MISRVis_rgborb = -1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   ;------------------------------------------------------------------------
   ; Reset to Defaults.
   ;------------------------------------------------------------------------

   (event.id EQ widget_struct.reset_button) : BEGIN

      MISRVis_rgbcam = !VAR.AutoWndw.MISRVis_rgbcam

      FOR color_ndx = 0, 2 DO BEGIN
         WIDGET_CONTROL, $
            widget_struct.cam_buttons[color_ndx, $
                          !VAR.AutoWndw.MISRVis_rgbcam[color_ndx]], $
                          /SET_BUTTON
      ENDFOR

   END

   ;------------------------------------------------------------------------
   ; Assign camera and band to color.  If the button is not OK, Cancel or
   ; Reset, then it is a camera button.
   ;------------------------------------------------------------------------

   ELSE : BEGIN
      IF (event.id GE widget_struct.cam_buttons[0,0] AND $
          event.id LE widget_struct.cam_buttons[2,8]) THEN BEGIN
         FOR color_ndx = 0, 2 DO BEGIN
            camera_ndx = WHERE(event.id EQ $
                               widget_struct.cam_buttons[color_ndx,*])
            IF (camera_ndx GE 0) THEN $
               MISRVis_rgbcam[color_ndx] = camera_ndx
         ENDFOR
      ENDIF
      IF (event.id GE widget_struct.bnd_buttons[0,0] AND $
          event.id LE widget_struct.bnd_buttons[2,3]) THEN BEGIN
         FOR color_ndx = 0, 2 DO BEGIN
            band_ndx = WHERE(event.id EQ $
                             widget_struct.bnd_buttons[color_ndx,*])
            IF (band_ndx GE 0) THEN $
               MISRVis_rgbbnd[color_ndx] = band_ndx
         ENDFOR
      ENDIF
   END

ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=widget_struct, /NO_COPY

END ; MISRVision_eh

;***************************************************************************
PRO MISRVision_gui, OrbitNums, Retval
;***************************************************************************
; User interface for setting configurable parameter values.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON misrvision, MISRVis_rgborb, MISRVis_rgbcam, MISRVis_rgbbnd

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = 0
base0 = WIDGET_BASE(GROUP_LEADER=WIDGET_INFO(TLB,/CHILD), /COLUMN, $
                    /MODAL, TITLE= 'MISR Vision', /TLB_KILL_REQUEST_EVENTS)

label00 = WIDGET_LABEL(base0, VALUE='Select which orbit to use.' )
orbit_base = WIDGET_BASE(base0, /ROW, /FRAME, /BASE_ALIGN_CENTER, /EXCLUSIVE)
orbit_btn1 = WIDGET_BUTTON(orbit_base, VALUE=OrbitNums[0])
orbit2 = OrbitNums[0]
IF (N_ELEMENTS(OrbitNums) GT 1) THEN orbit2 = OrbitNums[1]
orbit_btn2 = WIDGET_BUTTON(orbit_base, VALUE=orbit2)

IF (N_ELEMENTS(OrbitNums) EQ 1) THEN WIDGET_CONTROL, orbit_btn2, SENSITIVE=0

IF (MISRVis_rgborb EQ 0) THEN WIDGET_CONTROL, orbit_btn1, /SET_BUTTON
IF (MISRVis_rgborb EQ 1) THEN WIDGET_CONTROL, orbit_btn2, /SET_BUTTON

;---------------------------------------------------------------------------
; Set up some variables.
;---------------------------------------------------------------------------

color_planes_1 = ['Red','Grn','Blu']
camera_names_1 = STRUPCASE(!KON.Instr.CAM_NAMES)
band_names_1   = ['Blu','Grn','Red','Nir']

cam_buttons = LONARR(3,9)
bnd_buttons = LONARR(3,4)

base_box = LONARR(3)
base_cam = LONARR(3)
base_bnd = LONARR(3)

label0 = WIDGET_LABEL(base0, VALUE='Select a camera and band for each color plane.' )

;---------------------------------------------------------------------------
; Loop over the 3 display colors.
;---------------------------------------------------------------------------

FOR color_ndx = 0, 2 DO BEGIN

   base_box[color_ndx] = WIDGET_BASE( base0, /COLUMN, /FRAME)
   label = WIDGET_LABEL(base_box[color_ndx], $
                        VALUE=color_planes_1[color_ndx]+' Color Plane')
   base_cam[color_ndx] = WIDGET_BASE( base_box[color_ndx], /ROW, /EXCLUSIVE)
   base_bnd[color_ndx] = WIDGET_BASE( base_box[color_ndx], /ROW, /EXCLUSIVE)

   ;------------------------------------------------------------------------
   ; Loop over the 9 cameras.
   ;------------------------------------------------------------------------

   FOR button_ndx = 0,!KON.Instr.NCAM-1 DO BEGIN

      ;----------------------------------------------------------------------
      ; Create each of the 3x9 camera radio buttons.
      ;----------------------------------------------------------------------

      cam_buttons[color_ndx,button_ndx] = $
          WIDGET_BUTTON(base_cam[color_ndx], VALUE=camera_names_1[button_ndx])

   ENDFOR

   ;------------------------------------------------------------------------
   ; Loop over the 4 bands.
   ;------------------------------------------------------------------------

   FOR button_ndx = 0, 3 DO BEGIN

      ;----------------------------------------------------------------------
      ; Create each of the 3x4 band radio buttons.
      ;----------------------------------------------------------------------

      bnd_buttons[color_ndx, button_ndx] = $
          WIDGET_BUTTON(base_bnd[color_ndx], VALUE=band_names_1[button_ndx])

   ENDFOR

   ;------------------------------------------------------------------------
   ; Set the default button for each camera and band.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, cam_buttons[color_ndx, $
                   !VAR.AutoWndw.MISRVis_rgbcam[color_ndx]], /SET_BUTTON

   WIDGET_CONTROL, bnd_buttons[color_ndx, MISRVis_rgbbnd[color_ndx]], $
                   /SET_BUTTON
ENDFOR

;---------------------------------------------------------------------------
; Create the OK, Cancel and Reset buttons.
;---------------------------------------------------------------------------

base1 = WIDGET_BASE( base0, /ROW, /ALIGN_CENTER)
ok_button = WIDGET_BUTTON( base1, VALUE='  OK  ')
cancel_button = WIDGET_BUTTON( base1, VALUE='Cancel')
reset_button = WIDGET_BUTTON( base1, VALUE='Reset to Defaults')

;---------------------------------------------------------------------------
; Keep track of the button id numbers.
;---------------------------------------------------------------------------

widget_struct = { $
   orbit_btn1    : orbit_btn1, $
   orbit_btn2    : orbit_btn2, $
   cam_buttons   : cam_buttons, $
   bnd_buttons   : bnd_buttons, $
   ok_button     : ok_button, $
   cancel_button : cancel_button, $
   reset_button  : reset_button }

WIDGET_CONTROL, base0, SET_UVALUE=widget_struct, /NO_COPY
WIDGET_CONTROL, base0, /REALIZE
XMANAGER, 'MISRVision_gui', base0, EVENT_HANDLER='MISRVision_eh'

IF (MISRVis_rgborb LT 0) THEN Retval = -1

END ; MISRVision_gui

;***************************************************************************
PRO Op_MISRVision, State, retval
;***************************************************************************
; 
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON misrvision, MISRVis_rgborb, MISRVis_rgbcam, MISRVis_rgbbnd
COMMON chan_minmax, ChanMinMax
COMMON coord_data, CoordStruct

   ;------------------------------------------------------------------------
   ; Don't allow if only red band data are loaded.
   ;------------------------------------------------------------------------

   IF (CoordStruct.(0).num_band EQ 1) THEN BEGIN
      mssg = ['This option is not available when', $
              'only red band data are loaded.']
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
;   !VAR.DataSwath.DATA_TYPE = 0

   ;------------------------------------------------------------------------
   ; Test if there are 2 orbits loaded.
   ;------------------------------------------------------------------------

   IF (State.nframes-1 LT 18) THEN BEGIN
      OrbitNums = [STRTRIM(STRING(CoordStruct.(0).OrbitNum),2)]
      MISRVis_rgborb = 0
   ENDIF ELSE BEGIN
      OrbitNums = [STRTRIM(STRING(CoordStruct.(0).OrbitNum),2), $
                   STRTRIM(STRING(CoordStruct.(1).OrbitNum),2)]
      MISRVis_rgborb = !VAR.AutoWndw.MISRVis_rgborb
   ENDELSE

   ;------------------------------------------------------------------------
   ; Run the MISR Vision Graphical User Interface.
   ;------------------------------------------------------------------------

   MISRVis_rgbcam = !VAR.AutoWndw.MISRVis_rgbcam
   MISRVis_rgbbnd = !VAR.AutoWndw.MISRVis_rgbbnd

   MISRVision_gui, OrbitNums, retval
   IF (retval LT 0) THEN RETURN
   
   !VAR.AutoWndw.MISRVis_rgborb = MISRVis_rgborb
   !VAR.AutoWndw.MISRVis_rgbcam = MISRVis_rgbcam
   !VAR.AutoWndw.MISRVis_rgbbnd = MISRVis_rgbbnd

   WIDGET_CONTROL, /HOURGLASS

   ;------------------------------------------------------------------------
   ; Loop over each of the 3 colors.
   ;------------------------------------------------------------------------

   FOR color_ndx = 0, 2 DO BEGIN

      ;----------------------------------------------------------------------
      ; Copy the MISR Vision image into work image.
      ;----------------------------------------------------------------------

      cam_num = MISRVis_rgbcam[color_ndx]
      IF (MISRVis_rgborb EQ 1) THEN cam_num += 9

      bnd_num = MISRVis_rgbbnd[color_ndx]
      bnd_num = CoordStruct.(0).band_ndx[bnd_num]

      WorkImage[*,*,color_ndx] = GetRawImage(-1, -1, -1, -1, $
                                             bnd_num, cam_num, $
                                             !KON.Instr.HI_RES_PIX_SIZE, $
                                             !KON.Misc.INTERP_TYPE_SAMP)

      ;----------------------------------------------------------------------
      ; Use existing minmax values for the work image.
      ;----------------------------------------------------------------------

      WORK_MinMax[color_ndx,0] = ChanMinMax[0,color_ndx,cam_num]
      WORK_MinMax[color_ndx,1] = ChanMinMax[1,color_ndx,cam_num]

      ;----------------------------------------------------------------------
      ; Display the MISR Vision work image.
      ;----------------------------------------------------------------------

      TV, BYTSCL(WorkImage[*,*,color_ndx], $
                 MIN=WORK_MinMax[color_ndx,0], $
                 MAX=WORK_MinMax[color_ndx,1],/NAN), $
                 ORDER=1, CHANNEL=color_ndx+1

   ENDFOR

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData


END ; OP_MISRVision

;***************************************************************************
PRO Op_OrbitDiff, State, Cam, Retval
;***************************************************************************
; Routine displays a color- or gray-scale image of the difference between
; identical scenes in the same camera for 2 different orbits. The resulting
; image is displayed in the 0-based, camera 0 window referred to as the
; !VAR.WORK_WNDW. Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Only proceed if there are 2 orbits loaded.
   ;------------------------------------------------------------------------

   IF ((State.nframes-1) LT 18) THEN BEGIN
      ival = DIALOG_MESSAGE( $
              'You must have 2 orbits loaded to use this option.', $
              /INFORMATION, /CENTER)
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxDiffForScaling = 5.0

   ;------------------------------------------------------------------------
   ; Set up the camera indices. Proceed differently depending on the number
   ; of bands loaded. If user selected to difference bands in development
   ; version (Red - NIR), subtract the extra value of 10 passed as the cue.
   ;------------------------------------------------------------------------

   DoBandDiff = 0
   IF (Cam GT 9) THEN BEGIN
      DoBandDiff = 1
      Cam = Cam - 10
   ENDIF

   cam1 = Cam
   cam2 = Cam + 9

   IF (CoordStruct.(0).num_band EQ 1 OR $
       CoordStruct.(1).num_band EQ 1) THEN BEGIN

      WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, cam2, $
                                     !KON.Instr.HI_RES_PIX_SIZE, $
                                     !KON.Misc.INTERP_TYPE_SAMP) - $
                         GetRawImage(-1, -1, -1, -1, 0, cam1, $
                                     !KON.Instr.HI_RES_PIX_SIZE, $
                                     !KON.Misc.INTERP_TYPE_SAMP)
      WorkImage[*,*,1] = WorkImage[*,*,0]
      WorkImage[*,*,2] = WorkImage[*,*,0]
   ENDIF ELSE BEGIN
      IF (DoBandDiff EQ 1) THEN BEGIN
          WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 3, cam1, $
                                         !KON.Instr.HI_RES_PIX_SIZE, $
                                         !KON.Misc.INTERP_TYPE_SAMP) - $
                             GetRawImage(-1, -1, -1, -1, 2, cam1, $
                                         !KON.Instr.HI_RES_PIX_SIZE, $
                                         !KON.Misc.INTERP_TYPE_SAMP)
         WorkImage[*,*,1] = WorkImage[*,*,0]
         WorkImage[*,*,2] = WorkImage[*,*,0]
      ENDIF ELSE BEGIN
         WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, cam2, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP) - $
                            GetRawImage(-1, -1, -1, -1, 0, cam1, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP)
         WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 1, cam2, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP) - $
                            GetRawImage(-1, -1, -1, -1, 1, cam1, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP)
         WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 2, cam2, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP) - $
                            GetRawImage(-1, -1, -1, -1, 2, cam1, $
                                        !KON.Instr.HI_RES_PIX_SIZE, $
                                        !KON.Misc.INTERP_TYPE_SAMP)
      ENDELSE
   ENDELSE

   goodpts = WHERE(WorkImage GT -MaxDiffForScaling AND $
                   WorkImage LT  MaxDiffForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      ival = DIALOG_MESSAGE('Not enough good data to continue. ' + $
                            'Quitting operation.', $
                            /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev  = STDDEV(WorkImage[goodpts])
   maxval  = MAX(WorkImage[goodpts],MIN=minval)

   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = WORK_MinMax[0,*]
   WORK_MinMax[2,*] = WORK_MinMax[0,*]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_OrbitDiff

;***************************************************************************
PRO Op_BandDiff, State, Cam, Bands, Retval
;***************************************************************************
; Routine displays a gray-scale image of the difference between identical
; scenes in the same camera for 2 different bands. The resulting image is
; displayed in window 0 referred to as the !VAR.WORK_WNDW.
; Creation Date: 7/29/11 by David L. Nelson
;---------------------------------------------------------------------------

COMMON coord_data, CoordStruct
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Don't allow if only red band data are loaded.
   ;------------------------------------------------------------------------

   IF (CoordStruct.(0).num_band EQ 1) THEN BEGIN
      mssg = ['This option is not available when', $
              'only red band data are loaded.']
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /INFO)
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxDiffForScaling = 0.5

   ;------------------------------------------------------------------------
   ; Set up the band indices.
   ;------------------------------------------------------------------------

   IF (Bands EQ 1) THEN BEGIN
      band1 = 2
      band2 = 1
   ENDIF
   IF (Bands EQ 2) THEN BEGIN
      band1 = 2
      band2 = 0
   ENDIF
   IF (Bands EQ 3) THEN BEGIN
      band1 = 2
      band2 = 3
   ENDIF

   ;------------------------------------------------------------------------
   ; Retrieve the data.
   ;------------------------------------------------------------------------

   WorkImage[*,*,0] = ABS(GetRawImage(-1, -1, -1, -1, band2, Cam, $
                                      !KON.Instr.HI_RES_PIX_SIZE, $
                                      !KON.Misc.INTERP_TYPE_SAMP) - $
                          GetRawImage(-1, -1, -1, -1, band1, Cam, $
                                      !KON.Instr.HI_RES_PIX_SIZE, $
                                      !KON.Misc.INTERP_TYPE_SAMP))

   WorkImage[*,*,1] = WorkImage[*,*,0]
   WorkImage[*,*,2] = WorkImage[*,*,0]

   goodpts = WHERE(WorkImage GT -MaxDiffForScaling AND $
                   WorkImage LT  MaxDiffForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      ival = DIALOG_MESSAGE('Not enough good data to continue. ' + $
                            'Quitting operation.', $
                            /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   minval = 0.0
   maxval = 0.97 * MAX(WorkImage[goodpts])
   goodpts = 0

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = WORK_MinMax[0,*]
   WORK_MinMax[2,*] = WORK_MinMax[0,*]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_BandDiff

;***************************************************************************
PRO Op_MeanStddevSkew, state
;***************************************************************************
; Routine displays a 3-color image computed from the mean, standard
; deviation and skewness of the BRF vs camera function. This is a proxy for
; the shape of the BRF curve.
;    red = mean
;    green = standard deviation
;    blue = skewness
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 5/1/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   Retval = -1

   rsize = SIZE(!VAR.RawImages)

   WorkImage[*,*,*] = 0.0

   FOR iline = 0, rsize[1]-1 DO BEGIN
      FOR isamp = 0, rsize[2]-1 DO BEGIN
         badpts = WHERE(!VAR.RawImages[iline,isamp,0,0:8] LE 0.0, numbad)
         IF (numbad EQ 0) THEN BEGIN
            stats = MOMENT(!VAR.RawImages[iline,isamp,0,0:8])
            WorkImage[iline,isamp,0] = stats[0]
            WorkImage[iline,isamp,1] = SQRT(stats[1])
            WorkImage[iline,isamp,2] = stats[2]
         ENDIF
      ENDFOR
   ENDFOR

   badpts = 0

   maxval0 = MAX(WorkImage[*,*,0],MIN=minval0)
   maxval1 = MAX(WorkImage[*,*,1],MIN=minval1)
   maxval2 = MAX(WorkImage[*,*,2],MIN=minval2)

   IF (maxval0 GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval0, MAX=maxval0, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval1, MAX=maxval1, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval2, MAX=maxval2, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval0,maxval0]
   WORK_MinMax[1,*] = [minval1,maxval1]
   WORK_MinMax[2,*] = [minval2,maxval2]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END

;***************************************************************************
PRO Op_MeanMeanMean, state
;***************************************************************************
; Routine displays a 3-color image computed from the means of 3 sets of
; camera combinations.  This is a proxy for the shape of the BRF curve.
;    red = mean of Df,Cf,Bf cameras
;    green = mean of Af,An,Aa cameras
;    blue = mean of Ba,Ca,Da cameras
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 5/1/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0
   Retval = -1

   WorkImage[*,*,0] = (GetRawImage(-1, -1, -1, -1, 0, 0, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 1, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 2, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP)) / 3.0
   WorkImage[*,*,1] = (GetRawImage(-1, -1, -1, -1, 0, 3, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 4, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 5, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP)) / 3.0
   WorkImage[*,*,2] = (GetRawImage(-1, -1, -1, -1, 0, 6, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 7, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP) + $
                       GetRawImage(-1, -1, -1, -1, 0, 8, $
                                   !KON.Instr.HI_RES_PIX_SIZE, $
                                   !KON.Misc.INTERP_TYPE_SAMP)) / 3.0

   goodpts = WHERE(WorkImage GT 0.0 AND $
                   WorkImage LT MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)

   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END

;***************************************************************************
PRO Op_QuadraticFit, state
;***************************************************************************
; Routine displays a 3-color image computed as the coefficients of the
; quadratic best fit to the BRF vs camera function.  This is a proxy for
; the shape of the BRF curve.
;    red = A (quadratic constant)
;    green = B (quadratic slope)
;    blue = C (quadratic curvature)
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 5/1/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   rsize = SIZE(!VAR.RawImages)

   WorkImage = TEMPORARY(WorkImage)*0.0

xxx = 2

IF (xxx EQ 1) THEN BEGIN

   zfac = rsize[1]*rsize[2]
   xfac = rsize[1]
   n_cams = 9
   ind_var = LINDGEN(n_cams)

   badpts = WHERE(!VAR.RawImages[*,*,0,0:8] LE 0.0, numbad)

   xbadpts = badpts MOD zfac MOD rsize[1]
   ybadpts = badpts MOD zfac / rsize[1]
   badpts  = 0
   xbadpts = xbadpts[UNIQ(xbadpts,SORT(xbadpts))]
   ybadpts = ybadpts[UNIQ(ybadpts,SORT(ybadpts))]

   tmp = LINDGEN(rsize[1],rsize[2],n_cams)
   tmp[xbadpts,ybadpts,0:n_cams-1] = (-1L)

   tmp2 = WHERE(tmp GE 0L,cnt)

   IF cnt LE 0 THEN RETURN

   good_idx = tmp[tmp2]
   tmp = 0 & tmp2 = 0

   FOR j=0L, cnt/n_cams - 1 DO BEGIN
      WorkImage[good_idx[j] MOD zfac MOD xfac, $
                good_idx[j] MOD zfac / xfac, 0:2] = $
          POLY_FIT(ind_var, !VAR.RawImages[ind_var*zfac+good_idx[j]],0,2)
   ENDFOR

ENDIF

IF (xxx EQ 2) THEN BEGIN
   FOR iline = 0, rsize[1]-1 DO BEGIN
      FOR isamp = 0,  rsize[2]-1 DO BEGIN
         badpts = WHERE(!VAR.RawImages[iline,isamp,0,0:8] LE 0.0, numbad)
         IF (numbad EQ 0) THEN BEGIN
            coef= POLY_FIT([0,1,2,3,4,5,6,7,8], $
                           !VAR.RawImages[iline,isamp,0,0:8],2)
            WorkImage[iline,isamp,0] = coef[0]
            WorkImage[iline,isamp,1] = coef[1]
            WorkImage[iline,isamp,2] = coef[2]
         ENDIF
      ENDFOR
   ENDFOR
ENDIF

badpts  = 0

maxval0 = MAX(WorkImage[*,*,0],MIN=minval0)
maxval1 = MAX(WorkImage[*,*,1],MIN=minval1)
maxval2 = MAX(WorkImage[*,*,2],MIN=minval2)

IF (maxval0 GT 0 AND maxval1 GT 0 AND maxval2 GT 0) THEN BEGIN
   TV, BYTSCL(WorkImage[*,*,0], MIN=minval0, MAX=maxval0, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval1, MAX=maxval1, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval2, MAX=maxval2, /NAN), $
       ORDER=1, CHANNEL=3
ENDIF

WORK_MinMax[0,*] = [minval0,maxval0]
WORK_MinMax[1,*] = [minval1,maxval1]
WORK_MinMax[2,*] = [minval2,maxval2]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

Retval = 0

END

;***************************************************************************
PRO Op_ForeDiff, State, BegCam, Retval
;***************************************************************************
; Routine displays a 3-color image computed from the differences of forward
; radiances.  This is a proxy for slope.
;    red   = (Df - Cf) or (Cf - Bf)
;    green = (Cf - Bf) or (Bf - Af)
;    blue  = (Bf - Af) or (Af - An)
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, state.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)

   goodpts = WHERE(WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END

;***************************************************************************
PRO Op_AftDiff, State, BegCam, Retval
;***************************************************************************
; Routine displays a 3-color image computed from the differences of aft
; radiances.  This is a proxy for slope.
;    red   = (Da - Ca) or (Ca - Ba)
;    green = (Ca - Ba) or (Ba - Aa)
;    blue  = (Ba - Aa) or (Aa - An)
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camA = 8 - BegCam

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camA,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)

   goodpts = WHERE(WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END

;***************************************************************************
PRO Op_ForeVsAftDiff, State, BegCam, Retval
;***************************************************************************
; Routine displays a 3-color image computed from the ratios of fore and aft
; radiance differences.
;    red   = (Df - Cf) / (Da - Ca) or (Cf - Bf) / (Ca - Ba)
;    green = (Cf - Bf) / (Ca - Ba) or (Bf - Af) / (Ba - Aa)
;    blue  = (Bf - Af) / (Ba - Aa) or (Af - An) / (Aa - An)
; The resulting image is displayed in the 0-based, camera 9 window referred
; to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 1.0

   camF = BegCam
   camA = 8 - BegCam

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camF+3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA-3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END

;***************************************************************************
PRO Op_ForeVsAftSum, State, BegCam, Retval
;***************************************************************************
; Routine displays a 3-color image computed from the ratios of fore and aft
; radiance sums.
;    red   = (Df + Cf) / (Da + Ca) or (Cf + Bf) / (Ca + Ba)
;    green = (Cf + Bf) / (Ca + Ba) or (Bf + Af) / (Ba + Aa)
;    blue  = (Bf + Af) / (Ba + Aa) or (Af + An) / (Aa + An)
; The resulting image is displayed in the 0-based, camera 9 position.
; This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 3.0

   camF = BegCam
   camA = 8 - BegCam

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camF+3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) + $
                      GetRawImage(-1, -1, -1, -1, 0, camA-3, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT 0.0 AND $
                   WorkImage LT MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0
END

;***************************************************************************
PRO Op_ForeVsAft, State, BegCam, Retval
;***************************************************************************
; Routine displays a 3-color image computed from the ratios of fore and aft
; radiances.
;    red   = Df / Da or Cf / Ca
;    green = Cf / Ca or Bf / Ba
;    blue  = Bf / Ba or Af / Aa
; The resulting image is displayed in the 0-based, camera 9 position.
; This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam
   camA = 8 - BegCam

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA,   $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 0, camF+1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 0, camF+2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA-2, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT 0.0 AND $
                   WorkImage LT MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0
END

;***************************************************************************
PRO Op_CamDivNadir, State, BegCam, Retval
;***************************************************************************
; Routine displays a ??
; The resulting image is displayed in the 0-based, camera 0 position.
; This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 2/18/04 by David L. Nelson
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam
   camAn = 4

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, BegCam, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camAn,  $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = WorkImage[*,*,0]
   WorkImage[*,*,2] = WorkImage[*,*,0]

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT 0.0 AND $
                   WorkImage LT MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0
END

;***************************************************************************
PRO Op_CamRatio_1, State, BegCam, Retval
;***************************************************************************
; Routine displays a ??
; The resulting image is displayed in the 0-based, camera 9 position.
; This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 3/15/04 by David L. Nelson, Jeffrey R. Hall
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 20.0

   camF = BegCam
   camA = 8 - camF

   WorkImage[*,*,0] = ( GetRawImage(-1, -1, -1, -1, 0, camF, $
                                    !KON.Instr.HI_RES_PIX_SIZE, $
                                    !KON.Misc.INTERP_TYPE_SAMP) - $
                        GetRawImage(-1, -1, -1, -1, 0, camA, $
                                    !KON.Instr.HI_RES_PIX_SIZE, $
                                    !KON.Misc.INTERP_TYPE_SAMP) ) / $
                      ( GetRawImage(-1, -1, -1, -1, 0, camF, $
                                    !KON.Instr.HI_RES_PIX_SIZE, $
                                    !KON.Misc.INTERP_TYPE_SAMP) + $
                        GetRawImage(-1, -1, -1, -1, 0, camA, $
                                    !KON.Instr.HI_RES_PIX_SIZE, $
                                    !KON.Misc.INTERP_TYPE_SAMP) )
   WorkImage[*,*,1] = WorkImage[*,*,0]
   WorkImage[*,*,2] = WorkImage[*,*,0]

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0
END

;***************************************************************************
PRO Op_CamRatio_2, State, BegCam, Retval
;***************************************************************************
; Routine displays a ??
; The resulting image is displayed in the 0-based, camera 9 position.
; This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 3/15/04 by David L. Nelson, Jeffrey R. Hall
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam
   camA = 8 - camF

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) / $
                      GetRawImage(-1, -1, -1, -1, 0, camA, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = WorkImage[*,*,0]
   WorkImage[*,*,2] = WorkImage[*,*,0]

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT 0.0 AND $
                   WorkImage LT MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_CamRatio_2

;***************************************************************************
PRO Op_CamDiff, State, BegCam, Retval
;***************************************************************************
; Routine creates an image that is the difference between the same camera
; in 2 different orbits from the same path. The resulting image is displayed
; in MINX camera position 0. This is referred to as the !VAR.WORK_WNDW.
; Creation Date: 3/15/04 by David L. Nelson, Jeffrey R. Hall.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam
   camA = 8 - camF

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, camF, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP) - $
                      GetRawImage(-1, -1, -1, -1, 0, camA, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   WorkImage[*,*,1] = WorkImage[*,*,0]
   WorkImage[*,*,2] = WorkImage[*,*,0]

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev
   IF (minval LT (meanval - 2.0 * stddev)) THEN $
       minval = meanval - 2.0 * stddev

   TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
       ORDER=1, CHANNEL=3

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_CamDiff


;**************************************************************************
PRO GetRedBlue3D_eh, event
;**************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON Options_RB3D, CamBtns, CamsSave, BrightSlider, BrightSave, $
                     BrightLabel, GrpHorz, nHorzGrp, HorzSave, GrpVert, $
                     nVertGrp, VertSave, OK_Btn, Cancel_Btn, CancelSave

;------------------------------------------------------------------------
; Handle the case if the user cancels via the system button.
;------------------------------------------------------------------------

IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
   CancelSave = 1
   WIDGET_CONTROL, event.top, /DESTROY
   RETURN
ENDIF
                  
;-----------------------------------------------------------------------
; Branch to the control that was clicked.
;-----------------------------------------------------------------------

CASE event.id OF

   CamBtns : BEGIN
   END
   BrightSlider : BEGIN
      WIDGET_CONTROL, BrightSlider, GET_VALUE=brightness
      Bval = STRTRIM(STRING(brightness),2)
      WIDGET_CONTROL, BrightLabel, SET_VALUE='Brightness as percent of ' + $
                      'default = ' + Bval
   END
   GrpHorz : BEGIN
   END
   GrpVert : BEGIN
   END

   OK_Btn : BEGIN
      WIDGET_CONTROL, CamBtns, GET_VALUE=campair_ndx
      CamsSave = campair_ndx
      WIDGET_CONTROL, BrightSlider, GET_VALUE=brightness
      BrightSave = brightness

      WIDGET_CONTROL, GrpHorz, GET_VALUE=horz_ndx
      IF (horz_ndx LT 1 OR horz_ndx GT nHorzGrp) THEN BEGIN
         strH = STRTRIM(STRING(nHorzGrp),2)
         mssg = 'Horizontal portion number must be in range 1 to ' + strH
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF
      HorzSave = horz_ndx - 1

      WIDGET_CONTROL, GrpVert, GET_VALUE=vert_ndx
      IF (vert_ndx LT 1 OR vert_ndx GT nVertGrp) THEN BEGIN
         strV = STRTRIM(STRING(nVertGrp),2)
         mssg = 'Vertical portion number must be in range 1 to ' + strV
         rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
         RETURN
      ENDIF
      VertSave = vert_ndx - 1

      CancelSave = 0
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   Cancel_Btn : BEGIN
      CancelSave = 1
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   ELSE:
ENDCASE

RETURN

END  ;  GetRedBlue3D_eh

;**************************************************************************
PRO GetRedBlue3D_gui, State, CamPairs, Brightness, GroupX, GroupY, Xmin, $
                      Xmax, Ymin, Ymax, Cancel
;**************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON coord_data, CoordStruct
   COMMON Options_RB3D, CamBtns, CamsSave, BrightSlider, BrightSave, $
                        BrightLabel, GrpHorz, nHorzGrp, HorzSave, GrpVert, $
                        nVertGrp, VertSave, OK_Btn, Cancel_Btn, CancelSave

   ;------------------------------------------------------------------------
   ; Initialize return values.
   ;------------------------------------------------------------------------

   CamsSave = CamPairs
   BrightSave = Brightness
   CancelSave = 0

   CamPairNames = ['Df-Cf', 'Cf-Bf', 'Bf-Af', 'Af-An', $
                   'An-Aa', 'Aa-Ba', 'Ba-Ca', 'Ca-Da', $
                   'Df-Da', 'Cf-Ca', 'Bf-Ba', 'Af-Aa']

   Bval = STRTRIM(STRING(BrightSave),2)

   ;------------------------------------------------------------------------
   ; Calculate the number of horizontal groups or vertical groups needed to
   ; show the entire loaded swath. This is a consequence of the 90 degree
   ; rotation of the image. 3 blocks fit well enough in the horizontal and
   ; vertical space available (center it so no-data regions on block edges
   ; are excluded); 4 rotated blocks fit nicely in the space; more than 4
   ; blocks do not fit in the horizontal space. 1 or 2 blocks do not fit
   ; in the vertical space. Prepare entries in the dialog box so user can
   ; specify which part of the swath to display.
   ;------------------------------------------------------------------------

   BlkBeg = CoordStruct.(0).BlkBeg
   BlkEnd = CoordStruct.(0).BlkEnd

   NumBlk = CoordStruct.(0).NumBlk

   ;------------------------------------------------------------------------
   ; Across-track coordinate groups pre-rotation.
   ;------------------------------------------------------------------------

   nHorzGrp = 1
   HorzBeg  = [0]
   HorzEnd  = [State.sizex < (NumBlk * 512)] - 1

   IF (NumBlk EQ 1) THEN BEGIN
      nHorzGrp = 4
      HorzBeg  = [  0,  512, 1024, 1536]
      HorzEnd  = [511, 1023, 1535, 2047]
   ENDIF

   IF (NumBlk EQ 2) THEN BEGIN
      nHorzGrp = 2
      HorzBeg  = [   0, 1024]
      HorzEnd  = [1023, 2047]
   ENDIF

   IF (NumBlk EQ 3) THEN BEGIN
      cntr = State.sizex / 2
      HorzBeg = [cntr-768]
      HorzEnd = [cntr+768-1]
   ENDIF

   strHorzGrp = STRTRIM(STRING(nHorzGrp),2)

   ;------------------------------------------------------------------------
   ; Along-track coordinate groups pre-rotation.
   ;------------------------------------------------------------------------

   nVertGrp = 1
   VertBeg  = [CoordStruct.(0).BlkBeg]
   VertEnd  = [CoordStruct.(0).BlkEnd]

   IF (NumBlk GE 5 AND NumBlk LE 8) THEN BEGIN
      nVertGrp = 2
      VertBeg  = [BlkBeg,   BlkEnd-3]
      VertEnd  = [BlkBeg+3, BlkEnd]
   ENDIF

   IF (NumBlk GE 9 AND NumBlk LE 12) THEN BEGIN
      nVertGrp = 3
      VertBeg  = [BlkBeg,   BlkBeg+NumBlk/2-2,   BlkEnd-3]
      VertEnd  = [BlkBeg+3, BlkBeg+NumBlk/2+2-1, BlkEnd]
   ENDIF

   IF (NumBlk GT 12) THEN BEGIN
      nVertGrp = 4
      VertBeg  = [BlkBeg,   BlkBeg+NumBlk/2-3, BlkBeg+NumBlk/2,   BlkEnd-3]
      VertEnd  = [BlkBeg+3, BlkBeg+NumBlk/2,   BlkBeg+NumBlk/2+3, BlkEnd]
   ENDIF

   strVertGrp = STRTRIM(STRING(nVertGrp),2)

   ;------------------------------------------------------------------------
   ; Define controls in widget.
   ;------------------------------------------------------------------------

   RB3d_base = WIDGET_BASE(TITLE=title, GROUP_LEADER=State.wDrawWindow, $
                           /COLUMN, /MODAL, /TLB_KILL_REQUEST_EVENTS)

   text_base = WIDGET_BASE(RB3d_base, /COLUMN, /ALIGN_CENTER, /FRAME)
   text1 = WIDGET_LABEL(text_base, VALUE='For optimum results, load 4 MISR blocks')
   text2 = WIDGET_LABEL(text_base, VALUE='into MINX. To view only clouds/aerosols')
   text3 = WIDGET_LABEL(text_base, VALUE='in 3D, use MISR terrain-projected data.')
   text4 = WIDGET_LABEL(text_base, VALUE='To also view the ground in 3D, use MISR')
   text5 = WIDGET_LABEL(text_base, VALUE='ellipsoid-projected data. Image will be')
   text6 = WIDGET_LABEL(text_base, VALUE='rotated counter-clockwise by 90 degrees.')
   text7 = WIDGET_LABEL(RB3d_base, VALUE=' ')
   text8 = WIDGET_LABEL(RB3d_base, VALUE='Select a camera pair to display:')

   CamBtns = CW_BGROUP(RB3d_base, CamPairNames, /EXCLUSIVE, /RETURN_INDEX, $
                       LABEL_TOP='', /FRAME, ROW=3, SET_VALUE=CamsSave)

   blank1 = WIDGET_LABEL(RB3d_base, VALUE=' ')
   BrightLabel = WIDGET_LABEL(RB3d_base, VALUE='Brightness as percent ' + $
                              'of default: ' + Bval)
   BrightSlider = WIDGET_SLIDER(RB3d_base, MINIMUM=1, MAXIMUM=300, $
                         SCROLL=5, VALUE=BrightSave, /SUPPRESS_VALUE)

   Grp_base = WIDGET_BASE(RB3d_base, /COLUMN, /ALIGN_CENTER)
   IF (GroupX GT nHorzGrp) THEN GroupX = 1
   GrpHorz = CW_FIELD(Grp_base, VALUE=GroupX, XSIZE=2, /INTEGER, $
                      TITLE='Select horizontal portion (1-' + strHorzGrp + '):')
   IF (GroupY GT nVertGrp) THEN GroupY = 1
   GrpVert = CW_FIELD(Grp_base, VALUE=GroupY, XSIZE=2, /INTEGER, $
                      TITLE='Select vertical portion (1-' + strVertGrp + '):')

   IF (nHorzGrp EQ 1) THEN WIDGET_CONTROL, GrpHorz, SENSITIVE=0
   IF (nVertGrp EQ 1) THEN WIDGET_CONTROL, GrpVert, SENSITIVE=0

   OC_base = WIDGET_BASE(RB3d_base, /ROW, /ALIGN_CENTER)
   OK_Btn = WIDGET_BUTTON(OC_base, /ALIGN_CENTER, SCR_XSIZE=60, VALUE='OK')
   Cancel_Btn = WIDGET_BUTTON(OC_base, /ALIGN_CENTER, SCR_XSIZE=60, $
                              VALUE='Cancel')

   ;------------------------------------------------------------------------
   ; Create dialog box.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, RB3d_base, XOFFSET=700, YOFFSET=300
   WIDGET_CONTROL, RB3d_base, /REALIZE

   XMANAGER, 'GetRedBlue3D_gui', RB3d_base, EVENT_HANDLER='GetRedBlue3D_eh'

   ;------------------------------------------------------------------------
   ; Return the values selected.
   ;------------------------------------------------------------------------

   Cancel = CancelSave

   IF (~ Cancel) THEN BEGIN
      CamPairs = CamsSave
      Brightness = BrightSave
      GroupX = HorzSave + 1
      GroupY = VertSave + 1
      Xmin = HorzBeg[HorzSave]
      Xmax = HorzEnd[HorzSave]
      Ymin = (VertBeg[VertSave] - BlkBeg) * 512
      Ymax = (VertEnd[VertSave] - BlkBeg + 1) * 512 - 1
   ENDIF
      
END  ;  GetRedBlue3D_gui

;***************************************************************************
PRO Op_RedBlue3D, State, Retval
;***************************************************************************
; Routine displays a red/blue image of 2 cameras from the same orbit that
; can be viewed in 3D with red/blue glasses in the MINX camera 0 position
; (!VAR.WORK_WNDW). Put Cam1 red band in red color plane and Cam2 red band
; in blue and green planes (to produce cyan). Because image must be rotated
; 90 degrees, the maximum across-track image size in pixels is the smaller
; of across-track window size or along-track image size. The maximum along-
; track image size in pixels is the smaller of along-track window size or
; across-track image size.
; User options are 1) brightness, 2) cameras to pair.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   Retval = -1

   ;------------------------------------------------------------------------
   ; Get user's display choices.
   ;------------------------------------------------------------------------

   CamPairs   = !VAR.BRFparms.RedBlue3D_Cams
   Brightness = !VAR.BRFparms.RedBlueBright
   GroupX     = !VAR.BRFparms.ShowGroupInX
   GroupY     = !VAR.BRFparms.ShowGroupInY

   GetRedBlue3D_gui, State, CamPairs, Brightness, GroupX, GroupY, $
                     Xmin, Xmax, Ymin, Ymax, Cancel
   IF (Cancel) THEN RETURN

   !VAR.BRFparms.RedBlue3D_Cams = CamPairs
   !VAR.BRFparms.RedBlueBright  = Brightness
   !VAR.BRFparms.ShowGroupInX   = GroupX
   !VAR.BRFparms.ShowGroupInY   = GroupY

   IF (CamPairs GE 0 AND CamPairs LE 7) THEN $
      Cams = [CamPairs, CamPairs+1]
   IF (CamPairs GE 8 AND CamPairs LE 11) THEN $
      Cams = [CamPairs-8, 16-CamPairs]

   ;------------------------------------------------------------------------
   ; Get a copy of the first image. Reverse the direction of the y axis and
   ; rotate 90 degrees counterclockwise.
   ;------------------------------------------------------------------------

   image_red = GetRawImage(Xmin, Xmax, Ymin, Ymax, 0, Cams[0], $
                           !KON.Instr.HI_RES_PIX_SIZE, $
                           !KON.Misc.INTERP_TYPE_SAMP)

   image_red = ROTATE(REVERSE(image_red, 2), 1)

   ;------------------------------------------------------------------------
   ; Find bad data values and maximum value to use for each band.
   ;------------------------------------------------------------------------

   goodpts = WHERE(~ FINITE(image_red, /INFINITY) AND $
                   ~ FINITE(image_red, /NAN) AND $
                   image_red GT 0.0 AND image_red LT 1.0, numgood)

   maxval_red = (MAX(image_red[goodpts]) * 2.0 + $
                MEAN(image_red[goodpts]) * 3.0) / 5.0

   goodpts = 0

   ;------------------------------------------------------------------------
   ; Get a copy of the second image. Reverse the direction of the y axis and
   ; rotate 90 degrees counterclockwise.
   ;------------------------------------------------------------------------

   image_blu = GetRawImage(Xmin, Xmax, Ymin, Ymax, 0, Cams[1], $
                           !KON.Instr.HI_RES_PIX_SIZE, $
                           !KON.Misc.INTERP_TYPE_SAMP)

   image_blu = ROTATE(REVERSE(image_blu, 2), 1)

   ;------------------------------------------------------------------------
   ; Get the acceptable rotated size and check against the red.
   ;------------------------------------------------------------------------

   size_blu = SIZE(image_blu)

   IF (((SIZE(image_blu))[1] NE (SIZE(image_red))[1]) OR $
       ((SIZE(image_blu))[2] NE (SIZE(image_red))[2])) THEN BEGIN
      mssg = 'The sizes of the 2 images are not the same. Quitting.'
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      image_red = 0
      image_blu = 0
      RETURN
   ENDIF

   ;------------------------------------------------------------------------
   ; Find bad data values and maximum value to use for each band.
   ;------------------------------------------------------------------------

   goodpts = WHERE(~ FINITE(image_blu, /INFINITY) AND $
                   ~ FINITE(image_blu, /NAN) AND $
                   image_blu GT 0.0 AND image_blu LT 1.0, numgood)

   maxval_blu = (MAX(image_blu[goodpts]) + $
                MEAN(image_blu[goodpts])) / 2.0

   goodpts = 0

   ;------------------------------------------------------------------------
   ; Centered the image in WorkImage window in the across direction and
   ; position at top in the along direction.
   ;------------------------------------------------------------------------

   xbeg = (State.sizex - (SIZE(image_red))[1]) / 2
   xend =  State.sizex - xbeg - 1

   YsizeWI = (SIZE(WorkImage))[2]
   y1 = YsizeWI - (Xmax-Xmin) - 1
   y2 = YsizeWI - 1

   WorkImage[*,*,*] = 0
   WorkImage[xbeg:xend,y1:y2,0] = image_red
   WorkImage[xbeg:xend,y1:y2,1] = image_blu
   WorkImage[xbeg:xend,y1:y2,2] = image_blu

   image_red = 0
   image_blu = 0

   ;------------------------------------------------------------------------
   ; Adjust the image brightness.
   ;------------------------------------------------------------------------

   WorkImage *= Brightness / 100.0

   ;------------------------------------------------------------------------
   ; Copy the image into the display buffer.
   ;------------------------------------------------------------------------

   TV, BYTSCL(WorkImage[*,*,0], MIN=0, MAX=maxval_red, /NAN), CHANNEL=1
   TV, BYTSCL(WorkImage[*,*,1], MIN=0, MAX=maxval_blu, /NAN), CHANNEL=2
   TV, BYTSCL(WorkImage[*,*,2], MIN=0, MAX=maxval_blu, /NAN), CHANNEL=3

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_RedBlue3D

;***************************************************************************
PRO Op_EdgeEnhance, State, EdgeType, BegCam, Retval
;***************************************************************************
; Routine performs edge enhancement using Roberts method (EdgeType=0) or the
; Sobel method (EdgeType=1).
; Apply to cameras Df,An,Da (BegCam=0) or Cf,An,Ca (BegCam=1).
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   MaxRatioForScaling = 10.0

   camF = BegCam
   camDelta = 4 - camF

   IF (EdgeType EQ 0) THEN BEGIN
      WorkImage[*,*,0] = $
          ROBERTS(GetRawImage(-1, -1, -1, -1, 0, camF, $
                              !KON.Instr.HI_RES_PIX_SIZE, $
                              !KON.Misc.INTERP_TYPE_SAMP))
      WorkImage[*,*,1] = $
          ROBERTS(GetRawImage(-1, -1, -1, -1, 0, camF+camDelta*1, $
                              !KON.Instr.HI_RES_PIX_SIZE, $
                              !KON.Misc.INTERP_TYPE_SAMP))
      WorkImage[*,*,2] = $
          ROBERTS(GetRawImage(-1, -1, -1, -1, 0, camF+camDelta*2, $
                              !KON.Instr.HI_RES_PIX_SIZE, $
                              !KON.Misc.INTERP_TYPE_SAMP))
   ENDIF ELSE BEGIN
      WorkImage[*,*,0] = $
          SOBEL(GetRawImage(-1, -1, -1, -1, 0, camF, $
                            !KON.Instr.HI_RES_PIX_SIZE, $
                            !KON.Misc.INTERP_TYPE_SAMP))
      WorkImage[*,*,1] = $
          SOBEL(GetRawImage(-1, -1, -1, -1, 0, camF+camDelta*1, $
                            !KON.Instr.HI_RES_PIX_SIZE, $
                            !KON.Misc.INTERP_TYPE_SAMP))
      WorkImage[*,*,2] = $
          SOBEL(GetRawImage(-1, -1, -1, -1, 0, camF+camDelta*2, $
                            !KON.Instr.HI_RES_PIX_SIZE, $
                            !KON.Misc.INTERP_TYPE_SAMP))   
   ENDELSE

   goodpts = WHERE(~ FINITE(WorkImage,/INFINITY) AND $
                   ~ FINITE(WorkImage,/NAN) AND $
                   WorkImage GT -MaxRatioForScaling AND $
                   WorkImage LT  MaxRatioForScaling, numgood)

   IF (numgood LE 0) THEN BEGIN
      mssg = 'Not enough good data to continue.  Quitting operation.'
      rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
      ResetSwathData
      RETURN
   ENDIF

   meanval = MEAN(WorkImage[goodpts])
   stddev = STDDEV(WorkImage[goodpts])
   maxval = MAX(WorkImage[goodpts],MIN=minval)
   goodpts = 0

   IF (maxval GT (meanval + 2.0 * stddev)) THEN $
       maxval = meanval + 2.0 * stddev

   IF (maxval GT 0) THEN BEGIN
      TV, BYTSCL(WorkImage[*,*,0], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=1
      TV, BYTSCL(WorkImage[*,*,1], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=2
      TV, BYTSCL(WorkImage[*,*,2], MIN=minval, MAX=maxval, /NAN), $
          ORDER=1, CHANNEL=3
   ENDIF

   WORK_MinMax[0,*] = [minval,maxval]
   WORK_MinMax[1,*] = [minval,maxval]
   WORK_MinMax[2,*] = [minval,maxval]

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0
END

;***************************************************************************
PRO Op_HiPassFilter, State, CurrCam, Option, Retval
;***************************************************************************
; Routine performs high-pass filtering using the convolution operator.
; Apply to the RGB bands of the current camera.
;---------------------------------------------------------------------------

COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON coord_data, CoordStruct

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;------------------------------------------------------------------------
   ; Reset swath display button and type.
   ;------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

   ;------------------------------------------------------------------------
   ; Get the BRF data for the current window.
   ;------------------------------------------------------------------------

   WorkImage[*,*,0] = GetRawImage(-1, -1, -1, -1, 0, CurrCam-1, $
                                  !KON.Instr.HI_RES_PIX_SIZE, $
                                  !KON.Misc.INTERP_TYPE_SAMP)
   IF (!KON.Instr.NBAND GE 3) THEN BEGIN
      WorkImage[*,*,1] = GetRawImage(-1, -1, -1, -1, 1, CurrCam-1, $
                                     !KON.Instr.HI_RES_PIX_SIZE, $
                                     !KON.Misc.INTERP_TYPE_SAMP)
      WorkImage[*,*,2] = GetRawImage(-1, -1, -1, -1, 2, CurrCam-1, $
                                     !KON.Instr.HI_RES_PIX_SIZE, $
                                     !KON.Misc.INTERP_TYPE_SAMP)
   ENDIF

   ;------------------------------------------------------------------------
   ; Create a kernal for a high-pass filter.
   ;------------------------------------------------------------------------

   kernalsize = [3,3]
   kernal = REPLICATE(-1., kernalsize[0], kernalsize[1])
   kernal[1,1] = 8.

   ;------------------------------------------------------------------------
   ; Apply the filter to the 3 bands.
   ;------------------------------------------------------------------------

   IF (!KON.Instr.NBAND EQ 1) THEN BEGIN
      WorkImage[*,*,0] = CONVOL(FLOAT(WorkImage[*,*,0]), kernal, /CENTER, $
                                /EDGE_TRUNCATE, INVALID=-1.0, /NORMALIZE, $
                                MISSING=!KON.Misc.BADVALUE_REAL)
      WorkImage[*,*,1] = WorkImage[*,*,0]
      WorkImage[*,*,2] = WorkImage[*,*,0]
   ENDIF ELSE BEGIN
      WorkImage[*,*,0] = CONVOL(FLOAT(WorkImage[*,*,0]), kernal, /CENTER, $
                                /EDGE_TRUNCATE, INVALID=-1.0, /NORMALIZE, $
                                MISSING=!KON.Misc.BADVALUE_REAL)
      WorkImage[*,*,1] = CONVOL(FLOAT(WorkImage[*,*,1]), kernal, /CENTER, $
                                /EDGE_TRUNCATE, INVALID=-1.0, /NORMALIZE, $
                                MISSING=!KON.Misc.BADVALUE_REAL)
      WorkImage[*,*,2] = CONVOL(FLOAT(WorkImage[*,*,2]), kernal, /CENTER, $
                                /EDGE_TRUNCATE, INVALID=-1.0, /NORMALIZE, $
                                MISSING=!KON.Misc.BADVALUE_REAL)
   ENDELSE

   ;------------------------------------------------------------------------
   ; Get scaling factors and display the results for the RGB option.
   ;------------------------------------------------------------------------

   IF (Option EQ 1) THEN BEGIN

      FOR iband=0,2 DO BEGIN
         band_img = REFORM(WorkImage[*,*,iband])
         goodpts = WHERE(~ FINITE(band_img,/INFINITY) AND $
                         ~ FINITE(band_img,/NAN) AND $
                         band_img GT !KON.Misc.BADVALUE_REAL, numgood)

         IF (numgood LE 0) THEN BEGIN
            mssg = 'Not enough good data to continue. Quitting operation.'
            rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
            ResetSwathData
            RETURN
         ENDIF

         meanval = MEAN(band_img[goodpts])
         stddev  = STDDEV(band_img[goodpts])
         maxval  = MAX(band_img[goodpts],MIN=minval)
         goodpts = 0

         IF (minval LT (meanval - 1.0 * stddev)) THEN $
             minval = meanval - 1.0 * stddev
         IF (maxval GT (meanval + 1.0 * stddev)) THEN $
             maxval = meanval + 1.0 * stddev

         IF (maxval GT 0.0) THEN $
            TV, BYTSCL(band_img, MIN=minval, MAX=maxval, /NAN), ORDER=1, $
                       CHANNEL=iband+1

         WORK_MinMax[iband,*] = [minval, maxval]
      ENDFOR

   ENDIF ELSE BEGIN

      IF (!KON.Instr.NBAND GE 3) THEN BEGIN

         blu_img = REFORM(WorkImage[*,*,0])
         red_img = REFORM(WorkImage[*,*,2])

         band_img = blu_img / red_img

         goodpts = WHERE(blu_img NE !KON.Misc.BADVALUE_REAL AND $
                         red_img NE !KON.Misc.BADVALUE_REAL AND $
                         red_img NE   0.0  AND $
                         ~ FINITE(band_img,/INFINITY) AND $
                         ~ FINITE(band_img,/NAN) AND $
                         band_img NE 1.0, numgood)

         IF (numgood LE 0) THEN BEGIN
            mssg = 'Not enough good data to continue.  Quitting operation.'
            rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
            ResetSwathData
            RETURN
         ENDIF

         medval = MEDIAN(band_img[goodpts], /EVEN)
         maxval = MAX(band_img[goodpts],MIN=minval)
         stddev = STDDEV(band_img[goodpts])

         minval = medval - 1.
         maxval = medval + 1.

;         nbins = 201
;         hist = HISTOGRAM(band_img[goodpts], NBINS=nbins, MIN=minval, $
;                          MAX=maxval)
;         binsize = (maxval - minval) / (nbins - 1)
;         bins = FINDGEN(N_ELEMENTS(hist)) * binsize + minval
;         PRINT, MIN(hist)
;         PRINT, bins
;         WINDOW, 7
;         PLOT, bins, hist, YRANGE=[MIN(hist)-1,MAX(hist)+1], PSYM=10, $
;               XTITLE='Bin Number', YTITLE='Density Per Bin'

         IF ((maxval - minval) GT 0.0) THEN BEGIN
            TV, BYTSCL(band_img, MIN=minval, MAX=maxval, /NAN), ORDER=1, $
                CHANNEL=1
            TV, BYTSCL(band_img, MIN=minval, MAX=maxval, /NAN), ORDER=1, $
                CHANNEL=2
            TV, BYTSCL(band_img, MIN=minval, MAX=maxval, /NAN), ORDER=1, $
                CHANNEL=3
         ENDIF

         WORK_MinMax[0,*] = [minval, maxval]
         WORK_MinMax[1,*] = [minval, maxval]
         WORK_MinMax[2,*] = [minval, maxval]

         blu_img = 0
         red_img = 0
         goodpts = 0

         msg = 'Do you want to print the filtered blue/red results ' + $
               'to a binary file?'
         rtn = DIALOG_MESSAGE(msg, /QUESTION, /CENTER)

         IF (STRUPCASE(rtn) EQ 'YES') THEN BEGIN
            orbstr = STRTRIM(STRING(CoordStruct.(0).OrbitNum),2)
            blks = STRTRIM(STRING(CoordStruct.(0).BlkBeg),2) + '-' + $
                   STRTRIM(STRING(CoordStruct.(0).BlkEnd),2)
            dim_size = SIZE(band_img[*,*,0])    
            sizes = STRTRIM(STRING(dim_size[1]),2) + 'x' + $
                    STRTRIM(STRING(dim_size[2]),2)     
            outfile = !SAV.WorkingDir + 'FilteredBlueRedRatio_O' + $
                      orbstr + '_' + $
                      !KON.Instr.camera_names[CurrCam] + '_B' + blks + $
                      '_' + sizes + '.bin'
            OPENW, unit, outfile, /GET_LUN
            WRITEU, unit, band_img[*,*,0]
            FREE_LUN, unit
            msg = ['Data were written to file:', outfile, $
                   'Array dimensions are: ' + $
                   STRTRIM(STRING(dim_size[1]),2) + $
                   ' X ' + STRTRIM(STRING(dim_size[2]),2)]
            rtn = DIALOG_MESSAGE(msg, /INFORMATION, /CENTER)
         ENDIF

      ENDIF

   ENDELSE

   band_img = 0

   ;------------------------------------------------------------------------
   ; Set the flag to indicate that something other than a Data Menu item
   ; changed the contents of the OP window.
   ;------------------------------------------------------------------------

   ResetSwathData

   Retval = 0

END  ;  Op_HiPassFilter

;***************************************************************************
PRO Op_ClassifyAS, State, retval
;***************************************************************************
; Routine uses RGI direction in color cube for multiple cameras per pixel to
; isolate pure aerosol color from pure background color. Technique modified
; from source below by Dave D and David N. "Chromatic Framework for Vision
; in bad Weather", Narasimhan and Nayar, ...
;---------------------------------------------------------------------------

COMMON AerosolBox, Wndw_Save

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

!VAR.AerRsrch.Show2D = 1
           ; 1 -> project to a plane using orthographic coordinate system
           ; 2 -> project to a plane using a spherical coordinate system
           ; 3 -> create an interactive 3D display of the RGI points

;---------------------------------------------------------------------------
; Reset swath display button and type.
;---------------------------------------------------------------------------

   WIDGET_CONTROL, State.wFramesShowProdBtn, SENSITIVE=0
   !VAR.DataSwath.DATA_TYPE = 0

;---------------------------------------------------------------------------
; If an iTool window already exists, delete it and start over.
;---------------------------------------------------------------------------

IF (!VAR.AerRsrch.iToolID GT 0) THEN BEGIN
   ITDELETE, !VAR.AerRsrch.iToolID
   !VAR.AerRsrch.iToolID = -1
ENDIF

;---------------------------------------------------------------------------
; Set the state values for classifying Aerosols.
;---------------------------------------------------------------------------

!SAV.Digitize.DIG_STATE = !KON.Digitize.DIG_STATE_ZERO
!VAR.AerRsrch.AerosolClassify = 1

;---------------------------------------------------------------------------
; Tell user it's OK to start constructing boxes in which to process pixels.
;---------------------------------------------------------------------------

IF (!VAR.AerRsrch.Show2D) THEN BEGIN
   mssg = ['You can now click points or draw boxes on the screen in which', $
           'aerosol classification will occur. For each 1.1 km subregion,', $
           '9 camera RGI values will be projected in spherical coordinates',$
           'onto the window. To exit this option, select "No".']
ENDIF ELSE BEGIN
   mssg = ['You can now click points or draw boxes on the screen in which', $
           'aerosol classification will occur. For each 1.1 km subregion,', $
           '9 camera RGI values will be posted on the unit sphere of', $
           'an RGI image cube. To exit this option, select "No".']
ENDELSE

rval = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)

IF ((STRUPCASE(rval) EQ 'NO')) THEN BEGIN
   !VAR.AerRsrch.AerosolClassify = 0
   IF (!VAR.AerRsrch.iToolID GT 0) THEN ITDELETE, !VAR.AerRsrch.iToolID
   !VAR.AerRsrch.iToolID = -1
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Create the window to draw vector intersections in.
;---------------------------------------------------------------------------

CreateAerosolBoxWindow, State, retval

Retval = 0

END  ;  Op_ClassifyAS

;***************************************************************************
PRO CreateAerosolBoxWindow, State, retval
;***************************************************************************
; Create the window for drawing results of aerosol color cube data. 
;---------------------------------------------------------------------------

COMMON AerosolBox, Wndw_Save

COMPILE_OPT IDL2, LOGICAL_PREDICATE

bckgrnd_save = !P.BACKGROUND
!P.BACKGROUND = 16777215

;---------------------------------------------------------------------------
; Create the window to draw vector intersections in.
;---------------------------------------------------------------------------

xsize = !KON.Misc.ScreenX / 2.5
ysize = xsize
xpos  = !KON.Misc.ScreenX - xsize - 30
ypos  = 30
title = 'RGI Vector Intersections w/ Unit Sphere'

;---------------------------------------------------------------------------
; Prepare the drawing surface.
;---------------------------------------------------------------------------

IF (!VAR.AerRsrch.Show2D EQ 1) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Prepare orthographic projection surface for drawing unit color vectors.
   ;------------------------------------------------------------------------

   WINDOW, /FREE, TITLE=title, XPOS=xpos, YPOS=ypos, XSIZE=xsize, YSIZE=ysize
   Wndw_Save = !D.WINDOW

   MAP_SET, 45., 45., 0., /ORTHOGRAPHIC, /ISOTROPIC, /GRID, COLOR=0, $
            LATDEL=45., LONDEL=45., LABEL=2

   crds = CONVERT_COORD([0.0,90.0,45.0], [0.0,0.0,90.0], [1.0,1.0,1.0], $
                 /DATA, /TO_DEVICE)

   XYOUTS, crds[0,0], crds[1,0], 'Red',   COLOR=255,      /DEVICE
   XYOUTS, crds[0,1], crds[1,1], 'Green', COLOR=65280,    /DEVICE
   XYOUTS, crds[0,2], crds[1,2], 'NIR',   COLOR=16711935, /DEVICE

ENDIF

IF (!VAR.AerRsrch.Show2D EQ 2) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Prepare the theta/phi surface for drawing unit color vectors.
   ;------------------------------------------------------------------------


ENDIF

IF (!VAR.AerRsrch.Show2D EQ 3) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Prepare the RGI cube for drawing the unit color vectors.
   ;------------------------------------------------------------------------

   ClipPlanes = [[-0.99999,0,0,0], [0,-0.99999,0,0], [0,0,-0.99999,0]]

   axis1 = [[0.,0.,0.], [1.,0.,0.], [1.,1.,1.]]

   IPLOT, axis1, IDENTIFIER=!VAR.AerRsrch.iToolID, VIEW_TITLE=title, $
          DIMENSIONS=[xsize,xsize], LOCATION=[xpos,ypos], $
          XRANGE=[-1,1], YRANGE=[-1,1], ZRANGE=[-1,1], $
          XTITLE='Red', YTITLE='Green', ZTITLE='NIR', $
          COLOR=[255,0,0], THICK=1, /DISABLE_SPLASH_SCREEN, $
          ANISOTROPIC_SCALE_3D=1.0, SCALE_ISOTROPIC=2, $
          /NO_SAVEPROMPT, XMAJOR=3, YMAJOR=3, ZMAJOR=3, $
          XMINOR=0, YMINOR=0, ZMINOR=0, CLIP_PLANES=ClipPlanes

   axis2 = [[0.,0.,0.], [0.,1.,0.], [1.,1.,1.]]
   IPLOT, axis2, OVERPLOT=!VAR.AerRsrch.iToolID, COLOR=[0,255,0]

   axis3 = [[0.,0.,0.], [0.,0.,1.], [1.,1.,1.]]
   IPLOT, axis3, OVERPLOT=!VAR.AerRsrch.iToolID, COLOR=[127,0,127]

ENDIF

!P.BACKGROUND = bckgrnd_save

Retval = 0

END  ;  CreateAerosolBoxWindow

;************************************************************************
PRO BegGetAerosolBox, wTopWorkBase, State, Event
;************************************************************************
; Get the x/y window coordinates for the start corner pixel of a box in
; which BRFs will be retrieved for RGI color direction processing. Actual
; MISR color planes used are NIR, red, green. Blue is affected too much
; by Rayleigh scattering.
;------------------------------------------------------------------------

COMMON AerosolBox, Wndw_Save

COMPILE_OPT IDL2, LOGICAL_PREDICATE

!VAR.AerRsrch.Xbeg = Event.x
!VAR.AerRsrch.Ybeg = Event.y

END  ;  BegGetAerosolBox

;************************************************************************
PRO EndGetAerosolBox, wTopWorkBase, State, Event
;************************************************************************
; Get the x/y window coordinates for the end corner pixel of a box in
; which BRFs will be retrieved for RGI color direction processing. Actual
; MISR color planes used are NIR, red, green. Blue is affected too much
; by Rayleigh scattering.
;------------------------------------------------------------------------

COMMON AerosolBox, Wndw_Save

COMPILE_OPT IDL2, LOGICAL_PREDICATE

colors = [[0,0,0], [255,0,0], [0,255,0], [0,0,255], [127,127,0], $
          [127,0,127], [0,127,127]]
symndx = [1,3,3,3,2,3,3,3,6]
symsiz = [0.2,0.15,0.15,0.15,0.2,0.15,0.15,0.15,0.2]
symthk = [1,3,3,3,2,3,3,3,1]

!VAR.AerRsrch.Xend = Event.x
!VAR.AerRsrch.Yend = Event.y

;------------------------------------------------------------------------
; First set the corners to be at multiples of 4 275 m pixels. Then get
; the min, max and number of 275 m pixels needed. Create arrays to hold
; the unscaled and scaled (to unit vector) BRF values.
; NOTE - the pixel coordinates returned by the mouse have y-origin at the
; bottom of the window, while MISR data have y-origin at top.
;------------------------------------------------------------------------

xmin = !VAR.AerRsrch.Xbeg < !VAR.AerRsrch.Xend
xmax = !VAR.AerRsrch.Xbeg > !VAR.AerRsrch.Xend
ymin = !VAR.AerRsrch.Ybeg > !VAR.AerRsrch.Yend  ; opposite of expected,
ymax = !VAR.AerRsrch.Ybeg < !VAR.AerRsrch.Yend  ; because inverted below

ymin = State.sizey - ymin - 1
ymax = State.sizey - ymax - 1

rem = xmin MOD 4
xmin -= rem
rem = xmax MOD 4
IF (rem NE 0) THEN xmax += (4 - rem)
IF (xmax EQ xmin) THEN xmax += 4
xmax -= 1

rem = ymin MOD 4
ymin -= rem
rem = ymax MOD 4
IF (rem NE 0) THEN ymax += (4 - rem)
IF (ymax EQ ymin) THEN ymax += 4
ymax -= 1

nx = ((xmax - xmin + 1) / 4) > 1
ny = ((ymax - ymin + 1) / 4) > 1

rgi_vals = FLTARR(3, nx, ny, 9)
rgi_vlin = FLTARR(3, nx*ny, 9)
rgi_unit = FLTARR(3, nx*ny, 9)

;------------------------------------------------------------------------
; Get the BRF data for 9 cameras from memory for the selected pixels.
;------------------------------------------------------------------------

FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

   ;---------------------------------------------------------------------
   ; Get the data for RGI bands for this camera.
   ; NOTE - bands are stored in this order: red, green, blue, NIR.
   ; Also note - pixel coordinates returned by the mouse have y-origin
   ; at bottom, whereas MISR data have y-origin at top.
   ;---------------------------------------------------------------------

   rgi_vals[0,*,*,icam] = GetRawImageAllAt1100(xmin, xmax, ymin, ymax, $
                                               0, icam)
   rgi_vals[1,*,*,icam] = GetRawImageAllAt1100(xmin, xmax, ymin, ymax, $
                                               1, icam)
   rgi_vals[2,*,*,icam] = GetRawImageAllAt1100(xmin, xmax, ymin, ymax, $
                                               3, icam)

   ;---------------------------------------------------------------------
   ; Linearize the x,y components.
   ;---------------------------------------------------------------------

   ixy = 0
   FOR ix=0,nx-1 DO BEGIN
      FOR iy=0,ny-1 DO BEGIN
         rgi_vlin[0,ixy,icam] = rgi_vals[0,ix,iy,icam]
         rgi_vlin[1,ixy,icam] = rgi_vals[1,ix,iy,icam]
         rgi_vlin[2,ixy,icam] = rgi_vals[2,ix,iy,icam]
         ixy += 1
      ENDFOR
   ENDFOR

   ;---------------------------------------------------------------------
   ; Construct a unit vector for each BRF.
   ;---------------------------------------------------------------------

   red = REFORM(rgi_vlin[0,*,icam])
   grn = REFORM(rgi_vlin[1,*,icam])
   nir = REFORM(rgi_vlin[2,*,icam])

   ndxs = WHERE(FINITE(red, /NAN), numndxs)
   IF (numndxs GT 0) THEN red[ndxs] = 0
   ndxs = WHERE(FINITE(grn, /NAN), numndxs)
   IF (numndxs GT 0) THEN grn[ndxs] = 0
   ndxs = WHERE(FINITE(nir, /NAN), numndxs)
   IF (numndxs GT 0) THEN nir[ndxs] = 0

   uvlen = 1
   IF ((SIZE(red))[1] EQ 1 AND (SIZE(grn))[1] EQ 1 AND $
       (SIZE(nir))[1] EQ 1) THEN $
      uvlen = SQRT(red*red + grn*grn + nir*nir)
   IF (uvlen EQ 0) THEN uvlen = 1

   rgi_unit[0, *, icam] = red / uvlen
   rgi_unit[1, *, icam] = grn / uvlen
   rgi_unit[2, *, icam] = nir / uvlen

ENDFOR

ndxs = 0

;------------------------------------------------------------------------
; Plot the color direction results.
;------------------------------------------------------------------------

;  project to a plane using an orthographic coordinate system

IF (!VAR.AerRsrch.Show2D EQ 1) THEN BEGIN

;------------------------------------------------------------------------
; Plot the color direction results.
;------------------------------------------------------------------------

   CATCH, error_status
   IF (error_status NE 0) THEN BEGIN
      CreateAerosolBoxWindow, State, retval
      GOTO, end_catch
   ENDIF

   SafeWSET, Wndw_Save, didit

end_catch: CATCH, /CANCEL

   spher_crds = FLTARR(3,nx*ny,9)

   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
      spher_crds[*,*,icam] = CV_COORD(/DEGREES, /TO_SPHERE, $
                                      FROM_RECT=REFORM(rgi_unit[*,*,icam]))
   ENDFOR

   FOR ipts=0,nx*ny-1 DO BEGIN
      !VAR.AerRsrch.iColor = !VAR.AerRsrch.iColor MOD 7
      colval = colors[0,!VAR.AerRsrch.iColor] + 256 * $
               colors[1,!VAR.AerRsrch.iColor] + 65536 * $
               colors[2,!VAR.AerRsrch.iColor]
      numgood = 0

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         IF (spher_crds[0,ipts,icam] LE 0.0 OR $
             spher_crds[1,ipts,icam] LE 0.0 OR $
             spher_crds[2,ipts,icam] LE 0.0) THEN CONTINUE
         pts = [[spher_crds[*,ipts,icam]], $
                [spher_crds[*,ipts,icam] + 0.000001]]
         PLOTS, REFORM(spher_crds[*,ipts,icam]), COLOR=colval, $
                PSYM=symndx[icam], SYMSIZE=symsiz[icam]*3, $
                THICK=symthk[icam]

         ndxs = WHERE(spher_crds[*,ipts,icam] LE 0.0, numndxs)
         IF (numndxs GT 0) THEN CONTINUE
         IF (numgood EQ 0) THEN BEGIN
            goodndx = [icam]
            numgood += 1
         ENDIF ELSE BEGIN
            goodndx = [goodndx, icam]
         ENDELSE
      ENDFOR

      PLOTS, REFORM(spher_crds[*,ipts,goodndx]), COLOR=colval
      !VAR.AerRsrch.iColor += 1

   ENDFOR

ENDIF

ndxs = 0

;  project to a R-G, R-I ot G-I plane using a polar coordinate system
IF (!VAR.AerRsrch.Show2D EQ 2) THEN BEGIN

   SafeWSET, Wndw_Save, didit

   polar_crds = FLTARR(3,nx*ny,9)

   FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
      polar_crds[*,*,icam] = CV_COORD(/DEGREES, /TO_SPHERE, $
                                      FROM_RECT=REFORM(rgi_unit[*,*,icam]))
   ENDFOR

ENDIF

;  create an interactive 3D display of the RGI points
IF (!VAR.AerRsrch.Show2D EQ 3) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Plot the points in the iTool window. Connect the points for each camera
   ; set with a line.
   ;------------------------------------------------------------------------

   FOR inum=0,nx*ny-1 DO BEGIN

      PRINT, ''
      PRINT, 'BRF values and RGI unit vector values by camera ' + $
             'at 275 m pixel (display!) = ', $
             STRSPLIT(STRING(xmin),2), ', ', $
             STRSPLIT(STRING(State.sizey - ymin - 1),2)
      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         PRINT, FORMAT='(2x,3(F5.3,2x),2x,3(F5.3,2x))', $
            REFORM(rgi_vlin[*,inum,icam]), REFORM(rgi_unit[*,inum,icam])
      ENDFOR

      !VAR.AerRsrch.iColor = !VAR.AerRsrch.iColor MOD 7
      colvals = REFORM(colors[*,!VAR.AerRsrch.iColor])
      rgivals = REFORM(rgi_unit[*,inum,*])

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         IF (rgivals[0,icam] LE 0.0 OR rgivals[1,icam] LE 0.0 OR $
             rgivals[2,icam] LE 0.0) THEN CONTINUE
         pts = [[rgivals[*,icam]], [rgivals[*,icam] + 0.000001]]
         IPLOT, pts, OVERPLOT=!VAR.AerRsrch.iToolID, /SCATTER, $
                USE_DEFAULT_COLOR=0, SYM_COLOR=colvals, $
                SYM_INDEX=symndx[icam], SYM_SIZE=symsiz[icam], $
                SYM_THICK=symthk[icam]
      ENDFOR

      numgood = 0
      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         ndxs = WHERE(rgivals[*,icam] LE 0.0, numndxs)
         IF (numndxs GT 0) THEN CONTINUE
         IF (numgood EQ 0) THEN BEGIN
            goodndx = [icam]
            numgood += 1
         ENDIF ELSE BEGIN
            goodndx = [goodndx, icam]
         ENDELSE
      ENDFOR

      IPLOT, rgivals[*,goodndx], OVERPLOT=!VAR.AerRsrch.iToolID, $
             COLOR=colvals

      !VAR.AerRsrch.iColor += 1
   ENDFOR

ENDIF

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

red = 0
grn = 0
nir = 0
pts = 0
ndxs = 0
uvlen = 0
rgi_vals = 0
rgi_vlin = 0
rgi_unit = 0
polar_crds = 0

END  ;  EndGetAerosolBox
