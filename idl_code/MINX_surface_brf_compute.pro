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
PRO GetTerrainFileNames, ChannelMask, TerrainFile, terrain_files
;***************************************************************************
; function gets the full pathnames of all the requested Terrain
; products given any one of the pathnames; files are sought in the
; same directory and in subdirectories that mimic the local directory
; structure
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                       

  ;---------------------------------------------------------------------
  ; Initialize some variables.
  ;---------------------------------------------------------------------

  OrbitNum = LONARR(2)
  terrain_files[0] = TerrainFile
  cam_mask = INTARR(!KON.Instr.NCAM)

  ;---------------------------------------------------------------------
  ; Construct a camera mask from the channel mask.
  ;---------------------------------------------------------------------

  FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
     ndxgood = WHERE(ChannelMask[icam,*] NE 0, numgood)
     cam_mask[icam] = (numgood GT 0) ? 1 : 0
     ndxgood = 0
  ENDFOR

  ;---------------------------------------------------------------------
  ; Get the camera file names for the requested cameras.
  ;---------------------------------------------------------------------

  GetCamFiles, 0, terrain_files, OrbitNum, MISR_or_AirMISR, $
               !KON.Instr.NCAM, 0, cam_mask, Retval
  !VAR.CurrFiles.CamFiles[0:8] = terrain_files[0:8]

  Status = Retval

END

;***************************************************************************
PRO CreateChnlMask, IsLocalMode, ChannelFlags, ChannelMask
;***************************************************************************
; function creates a (camera x band) "mask" which indicates whether
; the channel will be written at hi-res or lo-res or not at all;
; 0 -> do not process; 1 -> write at hi-res; 2 -> write at lo-res
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END
                         
  AnCam = 4
  RedBand = 2

  ;---------------------------------------------------------------------
  ; Loop over the channels and enter the appropriate value.
  ;---------------------------------------------------------------------

  FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
     FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN

        IF (ChannelFlags[icam,iband] NE 0) THEN BEGIN
           IF (IsLocalMode EQ 1 OR $
               icam EQ AnCam OR iband EQ RedBand) THEN BEGIN
              ChannelMask[icam,iband] = 1
           ENDIF ELSE BEGIN
              ChannelMask[icam,iband] = 2
           ENDELSE
        ENDIF ELSE BEGIN
           ChannelMask[icam,iband] = 0
        ENDELSE

     ENDFOR
  ENDFOR

END

;***************************************************************************
PRO GetAsSurfaceFile, DirName, FileName, PathNum, OrbitNum, $
                      BlockBeg, BlockEnd, Retval
;***************************************************************************
; function added by D.Nelson, 4/05
; routine gets the name of an AS Surface file
; file name must conform to MISR naming conventions
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;-----------------------------------------------------------------------
; The user selects the file to use. Then get the directory and
; file name separately.
;-----------------------------------------------------------------------

FilenameFilter = ['MISR*LAND*.hdf']

GetLastFilename, 0, !KON.FileTyp.TypeLandbrf, FilenameFilter, 0, $
                 file_outpath, hdf_filename

IF (hdf_filename EQ '') THEN RETURN

DirName = file_outpath
nlen = STRLEN(DirName)
FileName = STRMID(hdf_filename, nlen)

;-----------------------------------------------------------------------
; Determine the orbit number.
;-----------------------------------------------------------------------

npos = STRPOS(FileName, '_O')
PathNum = FIX(STRMID(FileName, npos-3, 3))
OrbitNum = LONG(STRMID(FileName, npos+2, 6))

;-----------------------------------------------------------------------
; Find the first and last block number from the file.
;-----------------------------------------------------------------------

GetFirstLastBlocks, hdf_filename, BlockBeg, BlockEnd, Retval

;sd_id = HDF_StartInterface(hdf_filename)
;IF (sd_id LE 0) THEN RETURN
;
;dindex = HDF_SD_ATTRFIND(sd_id, 'Start_block')
;IF (dindex LE 0) THEN BEGIN
;   mssg = 'Could not find Start_block in ' + hdf_filename
;   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;   HDF_SD_END, sd_id
;   RETURN
;ENDIF
;HDF_SD_ATTRINFO, sd_id, dindex, DATA=BlockBeg
;
;dindex = HDF_SD_ATTRFIND(sd_id, 'End block')
;IF (dindex LE 0) THEN BEGIN
;   mssg = 'Could not find End block in ' + hdf_filename
;   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;   HDF_SD_END, sd_id
;   RETURN
;ENDIF
;HDF_SD_ATTRINFO, sd_id, dindex, DATA=BlockEnd
;
;HDF_SD_END, sd_id

END ; GetAsSurfaceFile

;***************************************************************************
PRO GetTerrainFile, PathNum, OrbitNum, BlockBeg, BlockEnd, $
                    DirName, FileName, Retval
;***************************************************************************
; function added by D.Nelson, 4/05
; routine gets the names of 1 Terrain camera file the user wants to
; use for converting TOA BRF to LandBRF
; file name must conform to MISR naming conventions
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  Retval = -1

  ; --------------------------------------------------------------------
  ; The user selects the camera to use. Only terrain files will do.
  ; Then get the directory and file name separately.
  ;--------------------------------------------------------------------

  FilenameFilter = ['MISR*TERRAIN*' + $
                    STRTRIM(STRING((OrbitNum)),2) + '*.hdf']

  GetLastFilename, 0, !KON.FileTyp.TypeL1B2Terrain, FilenameFilter, 0, $
                   file_outpath, hdf_filename

  IF (hdf_filename EQ '') THEN RETURN

  DirName = file_outpath
  nlen = STRLEN(DirName)
  FileName = STRMID(hdf_filename, nlen)

  ;--------------------------------------------------------------------
  ; Determine the orbit number and camera.  Orbit must be the same
  ; as for the AS Surface file.
  ;--------------------------------------------------------------------

  npos = STRPOS(FileName, '_O')
  path_num = FIX(STRMID(FileName, npos-3, 3))
  orbit_num = LONG(STRMID(FileName, npos+2, 6))

  IF (orbit_num NE OrbitNum) THEN BEGIN
    mmsg = ['Orbit number for Terrain file must', $
            'be the same as for AS Surface file.']
    rval = DIALOG_MESSAGE(mmsg, /ERROR, /CENTER)
    RETURN
  ENDIF

  ;--------------------------------------------------------------------
  ; Find the first and last block number from the file.  Block range
  ; requested for AS Surface file must be available in this Terrain
  ; file.
  ;--------------------------------------------------------------------

  GetFirstLastBlocks, hdf_filename, block_beg, block_end, Retval

;  sd_id = HDF_StartInterface(hdf_filename)
;  IF (sd_id LE 0) THEN RETURN
;
;  dindex = HDF_SD_ATTRFIND(sd_id, 'Start_block')
;  IF (dindex LE 0) THEN BEGIN
;     mssg = 'Could not find Start_block in ' + hdf_filename
;     rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;     HDF_SD_END, sd_id
;     RETURN
;  ENDIF
;  HDF_SD_ATTRINFO, sd_id, dindex, DATA=block_beg
;
;  dindex = HDF_SD_ATTRFIND(sd_id, 'End block')
;  IF (dindex LE 0) THEN BEGIN
;     mssg = 'Could not find End block in ' + hdf_filename
;     rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;     HDF_SD_END, sd_id
;     RETURN
;  ENDIF
;  HDF_SD_ATTRINFO, sd_id, dindex, DATA=block_end
;
;  HDF_SD_END, sd_id

  IF (block_beg GT BlockBeg OR block_end LT BlockEnd) THEN BEGIN
     mssg = 'Block range for AS Surface file must be available in Terrain file.'
     rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
     BlockBeg = BlockBeg > block_beg
     BlockEnd = BlockEnd < block_end
     Retval = -1
     RETURN
  ENDIF

END ; GetTerrainFile

;***************************************************************************
PRO GetAsSurfaceInfo, AsSurfaceFile, AS_Type, AS_Size_Cross, $
                      AS_Size_Along, AS_Block_Offsets, Status
;***************************************************************************
; Get AS Surface info on block offsets and dimensions and fill for
; the dataset of interest.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END
                         
  COMMON Surf_brfs, AS_Dims, AS_Scale, AS_Offset, AS_Fill
  Status = -1

  AS_Gridname = 'SubregParamsLnd'
  AS_Size_Cross_Attr = 'Block_size.resolution_y'
  AS_Size_Along_Attr = 'Block_size.resolution_x'

  AS_Scale_Attr  = 'Scale LandBRF'
  AS_Offset_Attr = 'Offset LandBRF'

  ;--------------------------------------------------------------------
  ; Open the scientific dataset interface and get dimension and
  ; fill info for the LandBRF dataset.  First check to confirm
  ; that the desired block range is present.
  ;--------------------------------------------------------------------

  si_id = HDF_StartInterface(AsSurfaceFile)
  IF (si_id LE 0) THEN RETURN

  sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.AS_Dataset)
  sd_id = HDF_SD_SELECT(si_id, sd_num)

  HDF_SD_GETINFO, sd_id, DIMS=AS_Dims, FILL=AS_Fill, TYPE=AS_Type
  HDF_SD_ENDACCESS, sd_id

  HDF_SD_END, si_id

  ;--------------------------------------------------------------------
  ; Open the grid interface and get scale and offset info for the
  ; LandBRF dataset.
  ;--------------------------------------------------------------------

  fid = EOS_GD_OPEN(AsSurfaceFile, /READ)
  gid = EOS_GD_ATTACH(fid, AS_Gridname)

  retval = EOS_GD_READATTR(gid, AS_Scale_Attr, scale)
  AS_Scale = scale[0]
  retval = EOS_GD_READATTR(gid, AS_Offset_Attr, offset)
  AS_Offset = offset[0]
  retval = EOS_GD_READATTR(gid, AS_Size_Cross_Attr, cross)
  AS_Size_Cross = cross[0]
  retval = EOS_GD_READATTR(gid, AS_Size_Along_Attr, along)
  AS_Size_Along = along[0]

  retval = EOS_GD_CLOSE(fid)

  ;--------------------------------------------------------------------
  ; Open the scientific dataset interface and get block offsets
  ; for all blocks for the LandBRF dataset.
  ;--------------------------------------------------------------------

  fid = HDF_OPEN(AsSurfaceFile, /READ)

  vd_ref = HDF_VD_FIND(fid, 'PerBlockMetadataCommon')
  vdata = HDF_VD_ATTACH(fid, vd_ref)
  nrec = HDF_VD_READ(vdata, along_coords, $
                     FIELDS='Block_coor_ulc_som_meter.y')
  HDF_VD_DETACH, vdata
  HDF_CLOSE, fid

  FOR iblk = 1, NBLK_ORBIT-1 DO BEGIN
     AS_Block_Offsets[iblk] = FIX((along_coords[BLOCK_BEG-1] - $
                                   along_coords[BLOCK_BEG+iblk-1]) / $
                                  (!KON.Instr.HI_RES_PIX_SIZE * 1000.0))
  ENDFOR

  min_offset = MIN(AS_Block_Offsets)
  AS_Block_Offsets -= min_offset
  off = FIX((along_coords[BLOCK_BEG] - $
             along_coords[BLOCK_BEG-1]) / $
            (!KON.Instr.HI_RES_PIX_SIZE * 1000.0))
  IF (BLOCK_BEG EQ BLOCK_END) THEN BEGIN
     AS_Block_Offsets[0] = 0
  ENDIF ELSE BEGIN
     AS_Block_Offsets[0] = AS_Block_Offsets[1] + off
  ENDELSE
  max_offset = MAX(AS_Block_Offsets)

  Status = 0

END

;***************************************************************************
PRO GetTerrainInfo, TerrainFile, TE_Type, TE_Scale_Type, TE_Size_Cross, $
                    TE_Size_Along, TE_Block_Offsets, Status
;***************************************************************************
; Get Terrain info on block offsets and dimensions and fill for
; the dataset of interest.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                     
  COMMON TOA_brfs, TE_Dims, TE_pix_size, TE_Scale_Fctr, $
                   TE_Fill, TE_Scale_Dims, TE_Scale_Fill

  Status = -1

  TE_Gridnames = ['BlueBand', 'GreenBand', 'RedBand', 'NIRBand']
  TE_Scale_Attr = 'Scale factor'
  TE_Size_Cross_Attr = 'Block_size.resolution_y'
  TE_Size_Along_Attr = 'Block_size.resolution_x'

  ;--------------------------------------------------------------------
  ; Open the scientific dataset interface and get dimension and
  ; fill info for the 4 Terrain band datasets.  First check to
  ; confirm that the desired block range is present.
  ;--------------------------------------------------------------------

  is_diff = 0
  dims2 = [[0,0,0], [0,0,0], [0,0,0], [0,0,0]]
  fill2 = 0
  type2 = 0

  si_id = HDF_StartInterface(TerrainFile)
  IF (si_id LE 0) THEN RETURN

  ;--------------------------------------------------------------------
  ; Get info for radiance datasets.
  ;--------------------------------------------------------------------

  FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
  
    sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.TE_Datasets[iband])
    sd_id = HDF_SD_SELECT(si_id, sd_num)

    HDF_SD_GETINFO, sd_id, DIMS=dims1, FILL=fill1, TYPE=type1
    HDF_SD_ENDACCESS, sd_id

    IF (iband GT 0) THEN BEGIN
      IF (dims1[2] NE dims2[2,iband-1] OR fill1 NE fill2 OR $
          type1 NE type2) THEN BEGIN
        mssg = 'Band attributes differ in ' + TerrainFile
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
      ENDIF
    ENDIF

    dims2[*,iband] = dims1
    fill2 = fill1
    type2 = type1

  ENDFOR

  TE_Dims = dims2
  TE_Fill = fill1
  TE_Type = type1

  ;--------------------------------------------------------------------
  ; Get info for conversion factor datasets.
  ;--------------------------------------------------------------------

  FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
  
    sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.TE_Scale_Datasets[iband])
    sd_id = HDF_SD_SELECT(si_id, sd_num)

    HDF_SD_GETINFO, sd_id, DIMS=dims1, FILL=fill1, TYPE=type1
    HDF_SD_ENDACCESS, sd_id

    IF (iband GT 0) THEN BEGIN
      IF (dims1[2] NE dims2[2,iband-1] OR fill1 NE fill2 OR $
          type1 NE type2) THEN BEGIN
        mssg = 'Band attributes differ in ' + TerrainFile
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
      ENDIF
    ENDIF

    dims2[*,iband] = dims1
    fill2 = fill1
    type2 = type1

  ENDFOR

  TE_Scale_Dims = dims2
  TE_Scale_Fill = fill1
  TE_Scale_Type = type1

  HDF_SD_END, si_id

  ;--------------------------------------------------------------------
  ; Open the grid interface and get scale and offset info for the
  ; 4 band datasets.
  ;--------------------------------------------------------------------

  fid = EOS_GD_OPEN(TerrainFile, /READ)
  IF (fid LE 0) THEN BEGIN
    mssg = 'Could not open file ' + TerrainFile + '. Quitting.'
    rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
    RETURN
  ENDIF

  FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN

    gid = EOS_GD_ATTACH(fid, TE_Gridnames[iband])
    IF (gid LE 0) THEN BEGIN
      mssg = 'Could not open grid ' + TE_Gridnames[iband] + '. Quitting.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      retval = EOS_GD_CLOSE(fid)
      RETURN
    ENDIF

    retval = EOS_GD_READATTR(gid, TE_Scale_Attr, scale)
    TE_Scale_Fctr[iband] = scale[0]
    retval = EOS_GD_READATTR(gid, TE_Size_Cross_Attr, cross)
    TE_Size_Cross[iband] = cross[0]
    retval = EOS_GD_READATTR(gid, TE_Size_Along_Attr, along)
    TE_Size_Along[iband] = along[0]

  ENDFOR

  retval = EOS_GD_CLOSE(fid)

  ;--------------------------------------------------------------------
  ; Open the scientific dataset interface and get block offsets
  ; for all blocks for the Terrain dataset.
  ;--------------------------------------------------------------------

  fid = HDF_OPEN(TerrainFile, /READ)

  vd_ref = HDF_VD_FIND(fid, 'PerBlockMetadataCommon')
  vdata = HDF_VD_ATTACH(fid, vd_ref)
  nrec = HDF_VD_READ(vdata, along_coords, $
                     FIELDS='Block_coor_ulc_som_meter.y')
  HDF_VD_DETACH, vdata
  HDF_CLOSE, fid

  FOR iblk = 1, NBLK_ORBIT-1 DO BEGIN
     TE_Block_Offsets[iblk] = FIX((along_coords[BLOCK_BEG-1] - $
                                   along_coords[BLOCK_BEG+iblk-1]) / $
                                   (!KON.Instr.HI_RES_PIX_SIZE * 1000.0))
  ENDFOR

  min_offset = MIN(TE_Block_Offsets)
  TE_Block_Offsets -= min_offset
  off = FIX((along_coords[BLOCK_BEG] - along_coords[BLOCK_BEG-1]) / $
            (!KON.Instr.HI_RES_PIX_SIZE * 1000.0))
  IF (BLOCK_BEG EQ BLOCK_END) THEN BEGIN
     TE_Block_Offsets[0] = 0
  ENDIF ELSE BEGIN
     TE_Block_Offsets[0] = TE_Block_Offsets[1] + off
  ENDELSE
  max_offset = MAX(TE_Block_Offsets)

  Status = 0

END

;***************************************************************************
PRO LoadAsSurfaceBlock, AsSurfaceFile, BlockNum, Icam, Iband, $
                        AsDatablock, Status
;***************************************************************************
; Get the next block of data from the LandBRF file and scale it
; to create BRFs.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END
  COMMON Surf_brfs, AS_Dims, AS_Scale, AS_Offset, AS_Fill

  Status = -1

  ;--------------------------------------------------------------------
  ; Open the AS Surface file scientific dataset interface, select
  ; the LandBRF dataset and read one block of data.
  ;--------------------------------------------------------------------

  si_id = HDF_StartInterface(AsSurfaceFile)
  IF (si_id LE 0) THEN RETURN

  sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.AS_Dataset)
  sd_id = HDF_SD_SELECT(si_id, sd_num)

  HDF_SD_GETDATA, sd_id, AsDatablock, $
                  START=[Icam,Iband,0,0,BlockNum], $
                  STRIDE=[1,1,1,1,1], $
                  COUNT=[1,1,AS_Dims[2],AS_Dims[3],1]

  HDF_SD_ENDACCESS, sd_id
  HDF_SD_END, si_id

  AsDatablock = FLOAT(REFORM(AsDatablock))

  ;--------------------------------------------------------------------
  ; Convert the raw values to BRFs.
  ;--------------------------------------------------------------------

  ndx_good = WHERE(AsDatablock LT AS_Fill, COMPLEMENT=ndx_bad, $
                   NCOMPLEMENT=numbad, numgood)

  IF (numgood GT 0) THEN $
    AsDatablock[ndx_good] = AsDatablock[ndx_good] * AS_Scale + $
                                                    AS_Offset
  IF (numbad GT 0) THEN $
    AsDatablock[ndx_bad] = !KON.Misc.BADVALUE_REAL

  ndx_good = 0
  ndx_bad = 0

  Status = 0

END

;***************************************************************************
PRO LoadTerrainBlock, TerrainFile, BlockNum, Iband, TE_datablock, $
                      Status
;***************************************************************************
; Get the next block of data from the Terrain file and scale it
; to create BRFs.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                        
  COMMON TOA_brfs, TE_Dims, TE_pix_size, TE_Scale_Fctr, $
                   TE_Fill, TE_Scale_Dims, TE_Scale_Fill

  Status = -1

  LargestGoodVal = 16376  ; This is the largest valid data value
                          ; after unscaling the radiances.

  ;--------------------------------------------------------------------
  ; Open the Terrain file scientific dataset interface, select
  ; the correct band dataset and read one block of data.
  ;--------------------------------------------------------------------

  si_id = HDF_StartInterface(TerrainFile)
  IF (si_id LE 0) THEN RETURN

  sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.TE_Datasets[Iband])
  sd_id = HDF_SD_SELECT(si_id, sd_num)

  HDF_SD_GETDATA, sd_id, TE_datablock, $
                  START=[0,0,BlockNum], $
                  STRIDE=[1,1,1], $
                  COUNT=[TE_Dims[0,Iband],TE_Dims[1,Iband],1]

  HDF_SD_ENDACCESS, sd_id

  ;--------------------------------------------------------------------
  ; Eliminate large fill numbers and dropouts.  Temporarily set
  ; these to 0.
  ;--------------------------------------------------------------------

  ndx_bad1 = WHERE(TE_datablock GE TE_Fill, numbad1, /L64)

  IF (numbad1 GT 0) THEN TE_datablock[ndx_bad1] = 0

  ;--------------------------------------------------------------------
  ; Convert raw data values to DN by shifting 2 bits.  Only then
  ; find all soft-coded fill values (terrain dropouts etc.)
  ;--------------------------------------------------------------------

  TE_datablock = ISHFT(TEMPORARY(TE_datablock),-2)

  ndx_bad2 = WHERE(TE_datablock GT LargestGoodVal, numbad2, /L64)

  ;--------------------------------------------------------------------
  ; Convert to floats, unscale the radiances and finally reset the
  ; 0's from above to BADVALUE.
  ;--------------------------------------------------------------------

  TE_datablock = FLOAT(TE_datablock)

  TE_datablock *= TE_Scale_Fctr[Iband]

  IF (numbad2 GT 0) THEN TE_datablock[ndx_bad2] = !KON.Misc.BADVALUE_REAL
  IF (numbad1 GT 0) THEN TE_datablock[ndx_bad1] = !KON.Misc.BADVALUE_REAL

  ;--------------------------------------------------------------------
  ; Select the channel whose conversion factors will be read. If
  ; data are old it might not contain BRF Conversion Factors. In
  ; that case just return the scaled radiance values.
  ;--------------------------------------------------------------------

  sd_num = HDF_SD_NAMETOINDEX(si_id, !KON.FileTyp.TE_Scale_Datasets[Iband])
  sd_id = HDF_SD_SELECT(si_id, sd_num)

  IF (sd_id LE 0) THEN BEGIN
    mssg = ['BRF Conversion Factors not found.', $
            'You are probably using older data.', $
            'Using radiance values instead.']
    d = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
    Status = 0
    HDF_SD_END, si_id
    RETURN
  ENDIF

  ;--------------------------------------------------------------------
  ; Read the data.
  ;--------------------------------------------------------------------

  HDF_SD_GETDATA, sd_id, convfact, $
                  COUNT=[TE_Scale_Dims[0,Iband],TE_Scale_Dims[1,Iband],1],$
                  START=[0,0,BlockNum], $
                  STRIDE=[1,1,1]

  HDF_SD_ENDACCESS, sd_id
  HDF_SD_END, si_id

  convfact = REFORM(convfact)

  ;--------------------------------------------------------------------
  ; Stretch the conversion factors to match the resolution of the
  ; radiances.
  ;--------------------------------------------------------------------

  convfact_new = REBIN(convfact, TE_Dims[0,Iband], TE_Dims[1,Iband], $
                       /SAMPLE)

  ;--------------------------------------------------------------------
  ; Convert radiances to BRFs.
  ;--------------------------------------------------------------------

  ndx_good = WHERE(TE_datablock NE !KON.Misc.BADVALUE_REAL AND $
                   convfact_new GE 0.0, numgood, /L64)

  IF (numgood GT 0) THEN $
    TE_datablock[ndx_good] *= convfact_new[ndx_good]

  ndx_bad1 = 0
  ndx_bad2 = 0
  ndx_good = 0

  Status = 0

END

;***************************************************************************
PRO GetAsSurfaceSubset, AsDatablock, Ialong, Icross, NumBrfAlong, $
                        NumBrfCross, Surf_Brfs, Status
;***************************************************************************
; Get the next block subset of BRFs from the LandBRF data.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON Surf_brfs, AS_Dims, AS_Scale, AS_Offset, AS_Fill

  Status = -1

  ;--------------------------------------------------------------------
  ; Copy all pixel values in the AS Surface block subset to the
  ; subset array.
  ;--------------------------------------------------------------------

  ibeg_cross = NumBrfCross * Icross
  iend_cross = ibeg_cross + NumBrfCross - 1
  ibeg_along = NumBrfAlong * Ialong
  iend_along = ibeg_along + NumBrfAlong - 1

  Surf_Brfs = AsDatablock[ibeg_cross:iend_cross, ibeg_along:iend_along]

  Status = 0

END

;***************************************************************************
PRO GetTerrainSubset, TE_datablock, Iband, Ialong, Icross, $
                      NumBrfAlong, NumBrfCross, RegressSize, $
                      TOA_Brfs, Status
;***************************************************************************
; Get the next block subset of BRFs from the Terrain data.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON TOA_brfs, TE_Dims, TE_pix_size, TE_Scale_Fctr, $
                   TE_Fill, TE_Scale_Dims, TE_Scale_Fill

  Status = -1

  ;--------------------------------------------------------------------
  ; Extract the BRFs in the Terrain block subset and down sample
  ; if needed by averaging to match the resolution of LandBRF data.
  ;--------------------------------------------------------------------

  num_edge_TE = FIX(RegressSize / TE_pix_size[Iband])

  ibeg_cross = num_edge_TE * Icross
  iend_cross = ibeg_cross + num_edge_TE - 1
  ibeg_along = num_edge_TE * Ialong
  iend_along = ibeg_along + num_edge_TE - 1

  IF (NumBrfCross NE num_edge_TE OR NumBrfAlong NE num_edge_TE) THEN BEGIN
    brfs = TE_datablock[ibeg_cross:iend_cross, ibeg_along:iend_along]
    TOA_Brfs = REBIN(brfs, NumBrfCross, NumBrfAlong)
  ENDIF ELSE BEGIN
    TOA_Brfs = TE_datablock[ibeg_cross:iend_cross, ibeg_along:iend_along]
  ENDELSE

  brfs = 0

  Status = 0

END

;***************************************************************************
PRO PlotRegressionResults, BlockNum, CamName, BandNum, Intcpts, $
                           Slopes, Corrs, NumCross, NumAlong
;***************************************************************************
; Plot the interpolated intercept, slope and correlation coefficient
; values
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, BLOCK_BEG, BLOCK_END                        
   
;--------------------------------------------------------------------------
; Initialize parameters.
;--------------------------------------------------------------------------

wndw_xsiz = NumCross + 8
wndw_ysiz = NumAlong + 8
wndw_xpos = 20
wndw_ypos = 975 - NumAlong

red_ndx_O = R_ORIG & grn_ndx_O = G_ORIG & blu_ndx_O = B_ORIG
red_ndx_C = R_CURR & grn_ndx_C = G_CURR & blu_ndx_C = B_CURR
LOADCT, !KON.Colors.RainbowPalette
   
;-----------------------------------------------------------------------
; Plot intercept values.
;-----------------------------------------------------------------------

ndxgood = WHERE(Intcpts NE !KON.Misc.BADVALUE_REAL, numgood, /L64)

IF (numgood GT 0) THEN BEGIN

   minval = MIN(Intcpts[ndxgood], MAX=maxval)

   WINDOW, 11, TITLE='Intrcpts for Cam=' + CamName + $
           ', Bnd=' + !KON.Instr.BAND_NAMES[BandNum] + $
           ', Blk=' + STRTRIM(STRING(BlockNum+1),2) + $
           ', Min/Max=' + STRTRIM(STRING(minval),2) + ' / ' + $
                          STRTRIM(STRING(maxval),2), $
           XPOS=wndw_xpos, YPOS=wndw_ypos, $
           XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, RETAIN=2

   TV, BYTSCL(Intcpts, MIN=minval, MAX=maxval), /ORDER
ENDIF
   
;-----------------------------------------------------------------------
; Plot slope values.
;-----------------------------------------------------------------------

ndxgood = WHERE(Slopes NE !KON.Misc.BADVALUE_REAL, numgood, /L64)
  
IF (numgood GT 0) THEN BEGIN

   minval = MIN(Slopes[ndxgood], MAX=maxval)
   wndw_ypos -= 200

   WINDOW, 12, TITLE='Slopes for Cam=' + CamName + $
           ', Bnd=' + !KON.Instr.BAND_NAMES[BandNum] + $
           ', Blk=' + STRTRIM(STRING(BlockNum+1),2) + $
           ', Min/Max=' + STRTRIM(STRING(minval),2) + ' / ' + $
                          STRTRIM(STRING(maxval),2), $
           XPOS=wndw_xpos, YPOS=wndw_ypos, $
           XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, RETAIN=2

   TV, BYTSCL(Slopes, MIN=minval, MAX=maxval), /ORDER
ENDIF
   
;-----------------------------------------------------------------------
; Plot correlation coefficient values.
;-----------------------------------------------------------------------

ndxgood = WHERE(Corrs NE !KON.Misc.BADVALUE_REAL, numgood, /L64)

IF (numgood GT 0) THEN BEGIN

   minval = MIN(Corrs[ndxgood], MAX=maxval)
   wndw_ypos -= 200

   WINDOW, 13, TITLE='CorrCoef for Cam=' + CamName + $
           ', Bnd=' + !KON.Instr.BAND_NAMES[BandNum] + $
           ', Blk=' + STRTRIM(STRING(BlockNum+1),2) + $
           ', Min/Max=' + STRTRIM(STRING(minval),2) + ' / ' + $
                          STRTRIM(STRING(maxval),2), $
           XPOS=wndw_xpos, YPOS=wndw_ypos, $
           XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, RETAIN=2

   TV, BYTSCL(Corrs, MIN=minval, MAX=maxval), /ORDER
ENDIF
   
;-----------------------------------------------------------------------
; Require user to answer before continuing. Then delete windows.
;-----------------------------------------------------------------------

mssg = 'Press OK to continue.'
rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

SafeWDELETE, 11, didit
SafeWDELETE, 12, didit
SafeWDELETE, 13, didit

R_ORIG = red_ndx_O & G_ORIG = grn_ndx_O & B_ORIG = blu_ndx_O
R_CURR = red_ndx_C & G_CURR = grn_ndx_C & B_CURR = blu_ndx_C
red_ndx_O = 0 & grn_ndx_O = 0 & blu_ndx_O = 0
red_ndx_C = 0 & grn_ndx_C = 0 & blu_ndx_C = 0

ndxgood = 0
   
END

;***************************************************************************
PRO PlotBrfResults, OrbitNum, BlockNum, CamName, BandNum, OldBrfs, $
                    OldNumCross, OldNumAlong, NewBrfs, NewNumCross, $
                    NewNumAlong
;***************************************************************************
; Plot the Surface BRFs before and after regression.
; values
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                     

  MakeTiff = 0  ; Set to 1 to plot to TIFF file rather than screen.

  TiffFileOld = !SAV.WorkingDir + !KON.Misc.Slash + 'MINX' + $
                !KON.Misc.Slash + 'LandBRF.tif'
                ; Name of TIFF file for old data if not plotting to screen.
  TiffFileNew = !SAV.WorkingDir + !KON.Misc.Slash + 'MINX' + $
                !KON.Misc.Slash + 'ToaBRF.tif'
                ; Name of TIFF file for new data if not plotting to screen.

  IF (BandNum NE 2) THEN MakeTiff = 0

;----------------------------------------------------------------------
; Initialize parameters.
;----------------------------------------------------------------------

  ;--------------------------------------------------------------------
  ; Plot LandBRF data from Surface product.
  ;--------------------------------------------------------------------

  wndw_xsiz = OldNumCross + 8
  wndw_ysiz = OldNumAlong + 8
  wndw_xpos = 400
  wndw_ypos = 975 - OldNumAlong

  ndxgood = WHERE(OldBrfs LT 5., num_good)
  maxval1 = MAX(OldBrfs[ndxgood])

  ndxgood = WHERE(NewBrfs LT 5., num_good)
  maxval2 = MAX(NewBrfs[ndxgood])

  maxval = maxval1 > maxval2

  IF (MakeTiff) THEN BEGIN

    WINDOW, 11, TITLE='AS_LAND SurfBRF; Cam/Bnd= ' + $
            CamName + '/' + !KON.Instr.BAND_NAMES[BandNum] + $
            '; Orb/Blk= ' + STRTRIM(STRING(OrbitNum),2) + '/' + $
            STRTRIM(STRING(BlockNum+1),2), $
            XPOS=wndw_xpos, YPOS=wndw_ypos, $
            XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, /PIXMAP

    TV, BYTSCL(OldBrfs, MIN=0.0, MAX=maxval)

    TVLCT, rv,gv,bv, /GET   
    WRITE_TIFF, TiffFileOld, TVRD(), RED = rv, GREEN = gv, BLUE = bv 

  ENDIF ELSE BEGIN

    WINDOW, 11, TITLE='AS_LAND SurfBRF; Cam/Bnd= ' + $
            CamName + '/' + !KON.Instr.BAND_NAMES[BandNum] + $
            '; Orb/Blk= ' + STRTRIM(STRING(OrbitNum),2) + '/' + $
            STRTRIM(STRING(BlockNum+1),2), $
            XPOS=wndw_xpos, YPOS=wndw_ypos, $
            XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, RETAIN=2

    TV, BYTSCL(OldBrfs, MIN=0.0, MAX=maxval), /ORDER

  ENDELSE

  ;--------------------------------------------------------------------
  ; Plot Surface BRFS from TOA regression.
  ;--------------------------------------------------------------------

  wndw_xsiz = NewNumCross + 8
  wndw_ysiz = NewNumAlong + 8
  wndw_xpos = (wndw_xsiz GT 800) ? -250 : 400
  wndw_ypos -= 100 + wndw_ysiz

  IF (MakeTiff) THEN BEGIN

    WINDOW, 12, TITLE='Regressed SurfBRF; Cam/Blk= ' + $
            CamName + '/' + !KON.Instr.BAND_NAMES[BandNum] + $
            '; OrbBlk= ' + STRTRIM(STRING(OrbitNum),2) + '/' + $
            STRTRIM(STRING(BlockNum+1),2), $
            XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, /PIXMAP

    TV, BYTSCL(NewBrfs, MIN=0.0, MAX=maxval)

    TVLCT, rv,gv,bv, /GET   
    WRITE_TIFF, TiffFileNew, TVRD(), RED = rv, GREEN = gv, BLUE = bv 

  ENDIF ELSE BEGIN

    WINDOW, 12, TITLE='Regressed SurfBRF; Cam/Blk= ' + $
            CamName + '/' + !KON.Instr.BAND_NAMES[BandNum] + $
            '; OrbBlk= ' + STRTRIM(STRING(OrbitNum),2) + '/' + $
            STRTRIM(STRING(BlockNum+1),2), $
            XPOS=wndw_xpos, YPOS=wndw_ypos, $
            XSIZE=wndw_xsiz, YSIZE=wndw_ysiz, RETAIN=2

    TV, BYTSCL(NewBrfs, MIN=0.0, MAX=maxval), /ORDER

  ENDELSE 

  ;--------------------------------------------------------------------
  ; Require user to answer before continuing. Then delete windows.
  ;--------------------------------------------------------------------

  mssg = 'Press OK to continue.'
  rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

  SafeWDELETE, 11, didit
  SafeWDELETE, 12, didit

  ndxgood = 0

END

;***************************************************************************
PRO FillCoeffGrids, Intcpt, Slopes, NumCoefAlong, NumCoefCross, $
                    Status
;***************************************************************************
; Find a reasonable value for all empty grid cells in the two linear
; coefficient arrays.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                         

  Status = -1

  ;--------------------------------------------------------------------
  ; Keep looping until all grid cells are filled.
  ;--------------------------------------------------------------------

  FOR iter = 0, 999 DO BEGIN

    ;---------------------------------------------------------------------------
    ; If there are no unfilled grid cells, then we're done.
    ;---------------------------------------------------------------------------

    ndxbad = WHERE(Slopes[*,*,2] EQ !KON.Misc.BADVALUE_REAL, numbad, /L64)
    ndxbad = 0

    IF (numbad EQ 0) THEN BEGIN
      slopes2 = 0
      intcpt2 = 0
      Status = 0
      RETURN
    ENDIF

    ;---------------------------------------------------------------------------
    ; Make a duplicate array for each of the coefficient arrays.
    ;---------------------------------------------------------------------------

    intcpt2 = Intcpt[*,*,2]
    slopes2 = Slopes[*,*,2]

    ;---------------------------------------------------------------------------
    ; Loop over all the grid cells in the Slopes array looking for
    ; those filled with !KON.Misc.BADVALUE_REAL.  The Intcpts array has the
    ; identical pattern of unfilled grid cells.  In each pass place
    ; the new values in a duplicate array.
    ;---------------------------------------------------------------------------

    FOR ialong1 = 0, NumCoefAlong-1 DO BEGIN
      FOR icross1 = 0, NumCoefCross-1 DO BEGIN

        IF (Slopes[icross1,ialong1,2] EQ !KON.Misc.BADVALUE_REAL) THEN BEGIN
    
          ;-------------------------------------------------------
          ; Find all the filled grid cells in the 3x3 array
          ; surrounding the current cell.
          ;-------------------------------------------------------

          sum_slope = 0.0
          sum_ntcpt = 0.0 
          num_val = 0

          ibeg_along = 0 > (ialong1-1)
          iend_along = (ialong1+1) < (NumCoefAlong-1)

          FOR ialong2 = ibeg_along, iend_along DO BEGIN

            ibeg_cross = 0 > (icross1-1)
            iend_cross = (icross1+1) < (NumCoefCross-1)

            FOR icross2 = ibeg_cross, iend_cross DO BEGIN

              IF (Slopes[icross2,ialong2,2] NE $
                  !KON.Misc.BADVALUE_REAL) THEN BEGIN
                sum_slope += Slopes[icross2,ialong2,2]
                sum_ntcpt += Intcpt[icross2,ialong2,2]
                num_val += 1
              ENDIF

            ENDFOR
          ENDFOR

          IF (num_val GT 0) THEN BEGIN
            slopes2[icross1,ialong1] = sum_slope / num_val
            intcpt2[icross1,ialong1] = sum_ntcpt / num_val
          ENDIF

        ENDIF

      ENDFOR
    ENDFOR

    Slopes[*,*,2] = slopes2[*,*]
    Intcpt[*,*,2] = intcpt2[*,*]

  ENDFOR

END

;***********************************************************************
PRO ConvertBrfData, AsSurfaceFile, TerrainFile, PathNum, OrbitNum, $
                    BlockBeg, BlockEnd, ChannelFlags, RegressSize, $
                    DoSmooth, FilterWidth, DoShowCoef, DoShowBRFs, $
                    FileType, status
;***********************************************************************
; This is the starting point for BRF conversion. Call this from button
; on MINX main dialog.
;-----------------------------------------------------------------------
  
COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END
                        
  COMMON Surf_brfs, AS_Dims, AS_Scale, AS_Offset, AS_Fill
  COMMON TOA_brfs, TE_Dims, TE_pix_size, TE_Scale_Fctr, $
                   TE_Fill, TE_Scale_Dims, TE_Scale_Fill
  Status = -1

  SHOW_BRF_DIFF_HISTOGRAM = 1
  HIST_BINSIZE_1 = 0.01
  HIST_BINSIZE_2 = 0.001

  ;--------------------------------------------------------------------
  ; Define constants and common variables.
  ;--------------------------------------------------------------------

  PATH_NUM   = PathNum
  ORBIT_NUM  = OrbitNum
  BLOCK_BEG  = BlockBeg
  BLOCK_END  = BlockEnd
  NBLK_ORBIT = BLOCK_END - BLOCK_BEG + 1

  AS_Block_Offsets = INTARR(NBLK_ORBIT)
  TE_Block_Offsets = INTARR(NBLK_ORBIT)

  ;--------------------------------------------------------------------
  ; Description of some parameters passed from dialog box.
  ;--------------------------------------------------------------------

;  RegressSize = 17600.   ; Length of edge of square area over which
;                         ; each regression is performed.
;  DoSmooth = 1           ; 1 = apply smoothing filter to lo-res linear
;                         ; coefficients; 0 = do not.
;  FilterWidth = 3        ; Width of the side of the filter used to
;                         ; smooth the lo-res linear coefficients if
;                         ; DoSmooth == 1.
;  DoShowBRFs = 0         ; 1 = display images of before Land BRFs and
;                         ; after computed BRFs; 0 = do not.
;  DoShowCoef = 0         ; 1 = display images of linear regression
;                         ; coefficients; 0 = do not.
;  FileType = 2           ; code for type of output file to save data in:
;                         ; 1 = HDFEOS, 2 = HDF, 3 = ASCII

  OutputDirectory = !SAV.WorkingDir + !KON.Misc.Slash

  ;---------------------------------------------------------------------
  ; Determine from the passed camera file name if it's local mode.
  ;---------------------------------------------------------------------

  ipos1 = STRPOS(TerrainFile, !KON.Misc.Slash, /REVERSE_SEARCH)
  ipos2 = STRPOS(TerrainFile, '_GM_', ipos1+1)
  ipos3 = STRPOS(TerrainFile, '_LM_', ipos1+1)

  IF (ipos2 GE 0 AND ipos3 LT 0) THEN IsLocalMode = 0
  IF (ipos3 GE 0 AND ipos2 LT 0) THEN IsLocalMode = 1
  IF ((ipos2 LT 0 AND ipos3 LT 0) OR $
      (ipos2 GE 0 AND ipos3 GE 0)) THEN BEGIN
    mssg = ['Quitting because could not determine if file', $
            TerrainFile, $
            'is global mode or local mode from file name.']
    d = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
    Status = -1
    RETURN
  ENDIF

  ;--------------------------------------------------------------------
  ; Create channel mask containing processing instructions values
  ; for each channel.
  ;--------------------------------------------------------------------

  ChannelMask = INTARR(!KON.Instr.NCAM, !KON.Instr.NBAND)
  CreateChnlMask, IsLocalMode, ChannelFlags, ChannelMask

  ;--------------------------------------------------------------------
  ; Use the passed Terrain file name to get the full pathnames of
  ; all Terrain files to be processed.  Also determine if the
  ; file is local mode or global mode.
  ;--------------------------------------------------------------------

  IsLocalMode = 0
  terrain_files = STRARR(!KON.Instr.NCAM)
  GetTerrainFileNames, ChannelMask, TerrainFile, terrain_files

  ;--------------------------------------------------------------------
  ; Get info on dimensions, fill and offsets from AS Surface file.
  ;--------------------------------------------------------------------

  GetAsSurfaceInfo, AsSurfaceFile, AS_Type, AS_Size_Cross, $
                    AS_Size_Along, AS_Block_Offsets, Status

  IF (Status EQ -1) THEN RETURN

  IF (AS_Size_Cross NE AS_Size_Along) THEN BEGIN
    mssg = 'AS Surface pixel sizes along and across are different.'
    rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
    RETURN
  ENDIF

  ;--------------------------------------------------------------------
  ; Create an output file, either HDFEOS, HDF or ASCII that will
  ; contain Surface BRFs computed by this function.
  ;--------------------------------------------------------------------

  CreateOutputFile, FileType, OutputDirectory, ChannelMask, $
                    AS_Block_Offsets, terrain_files, $
                    AsSurfaceFile, BrfOutputFile, $
                    RegressSize, DoSmooth, FilterWidth, $
                    Status

  IF (Status EQ -1) THEN RETURN

  ;--------------------------------------------------------------------
  ; Loop over the cameras. Skip those that are not to be processed.
  ;--------------------------------------------------------------------

  FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN

    ndxgood = WHERE(ChannelMask[icam,*] NE 0, numgood)
    IF (numgood EQ 0) THEN CONTINUE

    CamName = !KON.Instr.CAM_NAMES[icam]

    ;---------------------------------------------------------------------------
    ; Get info on dimensions, fill and offsets from Terrain file.
    ; Continue only if block offsets are equivalent.
    ;---------------------------------------------------------------------------

    TE_Scale_Fctr = FLTARR(!KON.Instr.NBAND)
    TE_Size_Cross = INTARR(!KON.Instr.NBAND)
    TE_Size_Along = INTARR(!KON.Instr.NBAND)
    TE_pix_size   = INTARR(!KON.Instr.NBAND)

    GetTerrainInfo, terrain_files[icam], TE_Type, TE_Scale_Type, $
                    TE_Size_Cross, TE_Size_Along, $
                    TE_Block_Offsets, Status

    IF (Status NE 0) THEN RETURN

    ndx = WHERE(AS_Block_Offsets[0:NBLK_ORBIT-1] NE $
                TE_Block_Offsets[0:NBLK_ORBIT-1], nbad)
    IF (nbad GT 0) THEN BEGIN
      mssg = 'Block offsets differ between AS Surface and Terrain files.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
    ENDIF

    FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
      IF (TE_Size_Cross[iband] NE TE_Size_Along[iband]) THEN BEGIN
        sband = STRTRIM(STRING(iband),2)
        mssg = ['Terrain pixel sizes along and across', $
                'are different for band ' + sband]
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
      ENDIF

      TE_pix_size[iband] = TE_Size_Cross[iband]

    ENDFOR

    ;---------------------------------------------------------------------------
    ; Create three 3-block arrays to hold the linear regression
    ; coefficients for all the block subsets.
    ;---------------------------------------------------------------------------

    num_coef_cross = FIX(TE_Size_Cross[0] / RegressSize * TE_Dims[0,0])
    num_coef_along = FIX(TE_Size_Along[0] / RegressSize * TE_Dims[1,0])

    BRF_intcpts = FLTARR(num_coef_cross, num_coef_along, 3)
    BRF_slopes  = FLTARR(num_coef_cross, num_coef_along, 3)
    BRF_corrs   = FLTARR(num_coef_cross, num_coef_along, 3)

    ;---------------------------------------------------------------------------
    ; Create 2 arrays large enough to hold the TOA BRFs and the Surface
    ; BRFs, both at 1.1 km resolution, for one block subset. Only one
    ; regression square is needed at a time.
    ;---------------------------------------------------------------------------

    num_brf_cross = FIX(RegressSize / AS_Size_Cross)
    num_brf_along = FIX(RegressSize / AS_Size_Along)

    TOA_Brfs  = FLTARR(num_brf_cross, num_brf_along)
    Surf_Brfs = FLTARR(num_brf_cross, num_brf_along)

    ;---------------------------------------------------------------------------
    ; Loop over the bands. Skip those that are not to be processed.
    ;---------------------------------------------------------------------------

    FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN

      IF (ChannelMask[icam,iband] EQ 0) THEN CONTINUE

      ;----------------------------------------------------------------
      ; Create 3 arrays large enough to hold the 2 linear regression
      ; coefficients and the correlation coefficients, all at the
      ; resolution of the input Terrain data.
      ;----------------------------------------------------------------

      BRF_fine_intcpts = FLTARR(TE_Dims[0,iband], TE_Dims[1,iband])
      BRF_fine_slopes  = FLTARR(TE_Dims[0,iband], TE_Dims[1,iband])
      BRF_fine_corrs   = FLTARR(TE_Dims[0,iband], TE_Dims[1,iband])

      ;----------------------------------------------------------------
      ; Loop over the blocks requested by user.  Keep 3 blocks of
      ; linear coefficient data in memory so we can compute Surface
      ; BRFs for the center block using values from adjacent blocks
      ; to ensure smoothness.
      ;----------------------------------------------------------------

      FOR iblk = 0, NBLK_ORBIT-1 DO BEGIN

        block_num = BLOCK_BEG + iblk - 1

        ;--------------------------------------------------------------
        ; Load one block of LandBRF data properly scaled and
        ; converted to BRFs.
        ;--------------------------------------------------------------

        LoadAsSurfaceBlock, AsSurfaceFile, block_num, icam, iband, $
                            AS_datablock, Status

        ;--------------------------------------------------------------
        ; Load one block of Terrain data properly scaled and
        ; converted to BRFs.
        ;--------------------------------------------------------------

        LoadTerrainBlock, terrain_files[icam], block_num, iband, $
                          TE_datablock, Status

        ;--------------------------------------------------------------
        ; Loop over the block subsets.
        ;--------------------------------------------------------------

        FOR ialong = 0, num_coef_along-1 DO BEGIN
          FOR icross = 0, num_coef_cross-1 DO BEGIN

            ;-----------------------------------------------------
            ; Get AS LandBRF values for the current block subset.
            ;-----------------------------------------------------

            GetAsSurfaceSubset, AS_datablock, ialong, icross, $
                                num_brf_along, num_brf_cross, $
                                Surf_Brfs, Status

            ;-----------------------------------------------------
            ; Get Terrain BRF values for the current block subset.
            ;-----------------------------------------------------

            GetTerrainSubset, TE_datablock, iband, ialong, icross, $
                              num_brf_along, num_brf_cross, $
                              RegressSize, TOA_Brfs, Status

            ;-----------------------------------------------------
            ; Compute linear regression coefficients for BRF TOA
            ; vs. BRF Surface for this subset in the current block.
            ; Get a list of the non-fill values for each array.
            ; Then compute the coefficients for the regression.
            ;-----------------------------------------------------

            intcpt = !KON.Misc.BADVALUE_REAL
            slope  = !KON.Misc.BADVALUE_REAL
            corr   = 0.0

            ndxgood = WHERE(Surf_Brfs NE !KON.Misc.BADVALUE_REAL AND $
                             TOA_Brfs NE !KON.Misc.BADVALUE_REAL, numgood)
          
            IF (numgood GT 1) THEN BEGIN
              slope = REGRESS(TOA_Brfs[ndxgood], Surf_Brfs[ndxgood], $
                       CONST=intcpt, CORRELATION=corr)
            ENDIF

            BRF_intcpts[icross,ialong,2] = intcpt
            BRF_slopes [icross,ialong,2] = slope
            BRF_corrs  [icross,ialong,2] = corr

            ;-----------------------------------------------------
            ; If we have at least 2 blocks in memory or only 1 block
            ; was requested, then compute the Surface BRF at the
            ; resolution of the Terrain channel data.
            ;-----------------------------------------------------

            IF (iblk GT 0 OR NBLK_ORBIT EQ 1) THEN BEGIN

            ENDIF

          ENDFOR
        ENDFOR

        ;--------------------------------------------------------------
        ; Create a grid the size of Terrain grid and use nearest
        ; neighbor fill to scale the BRF_slopes grid up to this. 
        ; Then determine where indices are !KON.Misc.BADVALUE_REAL.
        ; These indices will be used after interpolation below to
        ; refill the interpolated arrays with !KON.Misc.BADVALUE_REAL.
        ;--------------------------------------------------------------

        bad_coef_grid = REBIN(BRF_slopes[*,*,2], TE_Dims[0,iband], $
                              TE_Dims[1,iband], /SAMPLE)

        ;--------------------------------------------------------------
        ; Create a grid the size of Terrain grid and use nearest
        ; neighbor fill to scale the LandBRF grid up to this. Then
        ; determine where indices are !KON.Misc.BADVALUE_REAL. These
        ; indices will be used after interpolation below to mask out
        ; clouds as in the Surface product.
        ;--------------------------------------------------------------

        bad_surf_grid = REBIN(AS_datablock, TE_Dims[0,iband], $
                              TE_Dims[1,iband], /SAMPLE)


        ;--------------------------------------------------------------
        ; Interpolate linear coefficients into subsets where there
        ; is no data value.  
        ;--------------------------------------------------------------

        FillCoeffGrids, BRF_intcpts, BRF_slopes, num_coef_along, $
                        num_coef_cross

        ;--------------------------------------------------------------
        ; Smooth the linear coefficients if desired.
        ;--------------------------------------------------------------

        IF (DoSmooth) THEN BEGIN
          BRF_intcpts[*,*,2] = SMOOTH(BRF_intcpts[*,*,2], $
                                      FilterWidth, /EDGE_TRUNCATE, $
                                      /NAN)
          BRF_slopes[*,*,2]  = SMOOTH(BRF_slopes[*,*,2],  $
                                      FilterWidth, /EDGE_TRUNCATE, $
                                      /NAN)
        ENDIF

        ;--------------------------------------------------------------
        ; Interpolate linear coefficients into the 3 arrays whose
        ; resolution is the same as the input Terrain grid.  Adjust
        ; the interpolation parameters so the new grid starts and
        ; ends at points symmetric to the start and end of the old
        ; grid.  Use cubic option to improve results.
        ;--------------------------------------------------------------

        int_cross = num_coef_cross / FLOAT(TE_Dims[0,iband])
        int_along = num_coef_along / FLOAT(TE_Dims[1,iband])
        beg_cross = 0.5 * (1.0 - int_cross)
        beg_along = 0.5 * (1.0 - int_along)

        BRF_fine_intcpts = INTERPOLATE(BRF_intcpts[*,*,2], $
                 FINDGEN(TE_Dims[0,iband]) * int_cross - beg_cross, $
                 FINDGEN(TE_Dims[1,iband]) * int_along - beg_along, $
                 /GRID, CUBIC=-0.5)
        BRF_fine_slopes = INTERPOLATE(BRF_slopes[*,*,2], $
                 FINDGEN(TE_Dims[0,iband]) * int_cross - beg_cross, $
                 FINDGEN(TE_Dims[1,iband]) * int_along - beg_along, $
                 /GRID, CUBIC=-0.5)
        BRF_fine_corrs = INTERPOLATE(BRF_corrs[*,*,2], $
                 FINDGEN(TE_Dims[0,iband]) * int_cross - beg_cross, $
                 FINDGEN(TE_Dims[1,iband]) * int_along - beg_along, $
                 /GRID, CUBIC=-0.5)

        ;--------------------------------------------------------------
        ; Reset the coefficient grids so data that were invalid
        ; before are invalid again.
        ; NOTE - Comment these out to plot all data in block.
        ;--------------------------------------------------------------

        badndx = WHERE(bad_coef_grid EQ !KON.Misc.BADVALUE_REAL OR $
                       bad_surf_grid EQ !KON.Misc.BADVALUE_REAL, numbad, $
                       /L64)
        IF (numbad GT 0) THEN BEGIN
          BRF_fine_intcpts[badndx] = !KON.Misc.BADVALUE_REAL
          BRF_fine_slopes[badndx] = !KON.Misc.BADVALUE_REAL
          BRF_fine_corrs[badndx] = !KON.Misc.BADVALUE_REAL
        ENDIF

        ;--------------------------------------------------------------
        ; Plot the interpolated coefficient data.
        ;--------------------------------------------------------------

        IF (DoShowCoef) THEN $
           PlotRegressionResults, block_num, CamName, iband, $
                                  BRF_fine_intcpts, BRF_fine_slopes, $
                                  BRF_fine_corrs, TE_Dims[0,iband], $
                                  TE_Dims[1,iband]

        ;--------------------------------------------------------------
        ; Compute new Surface BRFs at resolution of Terrain data by
        ; multiplying and summing the TOA_Brf with the coeff grids.
        ;--------------------------------------------------------------

        ndx_bad = WHERE((TE_Datablock EQ !KON.Misc.BADVALUE_REAL) OR $
                        (BRF_fine_slopes EQ !KON.Misc.BADVALUE_REAL) OR $
                        (BRF_fine_intcpts EQ !KON.Misc.BADVALUE_REAL), $
                         COMPLEMENT=ndx_good, NCOMPLEMENT=num_good, $
                         num_bad, /L64)

        new_ary = TE_datablock

        IF (num_good GT 0) THEN $
           new_ary[ndx_good] = BRF_fine_intcpts[ndx_good] + $
               TE_datablock[ndx_good] * BRF_fine_slopes[ndx_good]

        IF (num_bad GT 0) THEN new_ary[ndx_bad] = !KON.Misc.BADVALUE_REAL

        TE_datablock = new_ary

        ;--------------------------------------------------------------
        ; Plot the interpolated Surface BRF data before and after
        ; regression.
        ;--------------------------------------------------------------

        IF (DoShowBRFs) THEN $
           PlotBrfResults, OrbitNum, block_num, CamName, iband, $
                           AS_datablock, AS_Dims[2], AS_Dims[3], $
                           TE_datablock, TE_Dims[0,iband], $
                           TE_Dims[1,iband]

        ;--------------------------------------------------------------
        ; If requested, show a histogram of the post-interpolation
        ; surface BRFs. Next compute the mean values of interpolated
        ; regions for comparison with pre-interpolation regions.
        ; Finally show a histogram of the difference results.
        ;--------------------------------------------------------------

        IF (SHOW_BRF_DIFF_HISTOGRAM) THEN BEGIN

           ndxs = WHERE(TE_datablock GT 0.0)
           minval = MIN(TE_datablock[ndxs], MAX=maxval)
           minval = FIX(minval / HIST_BINSIZE_1) * HIST_BINSIZE_1
           maxval = FIX(maxval / HIST_BINSIZE_1) * HIST_BINSIZE_1

           hist = HISTOGRAM(TE_datablock[ndxs], BINSIZE=HIST_BINSIZE_1, $
                            LOCATIONS=bin_begs)  

           plotID = 'Orbit= ' + STRTRIM(STRING(OrbitNum),2) + $
                    '; Block= ' + STRTRIM(STRING(block_num),2) + $
                    '; Cam= ' + CamName + '; Band= ' + $
                    !KON.Instr.BAND_NAMES[iband]

           WINDOW, 11, XPOS=500, YPOS=500, XSIZE=500, YSIZE=500
           PLOT, bin_begs, hist, YRANGE=[0, MAX(hist)*1.05], PSYM = 10, $ 
                 TITLE='Histogram of Interpolated Surface BRFs', $
                 SUBTITLE=PlotID, XTITLE='SurfBRF Post', $
                 YTITLE='Counts per Bin', BACKGROUND=16777215, COLOR=0, $
                 XTICKLEN=1, YTICKLEN=1, XGRIDSTYLE=1, YGRIDSTYLE=1, $
                 XMARGIN=[12,3], YMARGIN=[5,3]

           IF (TE_Dims[0,iband] EQ 2048) THEN BEGIN
              mean_val_post = REBIN(TE_datablock, AS_Dims[2], AS_Dims[3], $
                                    /SAMPLE)
           ENDIF ELSE BEGIN
              mean_val_post = TE_datablock
           ENDELSE

           ndxs = WHERE(mean_val_post GT 0.0 AND AS_datablock GT 0.0)
           mean_diff = mean_val_post[ndxs] - AS_datablock[ndxs]

           minval = MIN(mean_diff, MAX=maxval)
           minval = FIX(minval / HIST_BINSIZE_2) * HIST_BINSIZE_2
           maxval = FIX(maxval / HIST_BINSIZE_2) * HIST_BINSIZE_2

           hist = HISTOGRAM(mean_diff, BINSIZE=HIST_BINSIZE_2, $
                            LOCATIONS=bin_begs)  

           plotID = 'Orbit= ' + STRTRIM(STRING(OrbitNum),2) + $
                    '; Block= ' + STRTRIM(STRING(block_num),2) + $
                    '; Cam= ' + CamName + '; Band= ' + $
                    !KON.Instr.BAND_NAMES[iband]

           WINDOW, 12, XPOS=1100, YPOS=500, XSIZE=500, YSIZE=500
           PLOT, bin_begs, hist, YRANGE=[0, MAX(hist)*1.05], PSYM = 10, $ 
                 TITLE='Histogram of Surface BRF Differences', $
                 SUBTITLE=PlotID, XTITLE='SurfBRF Post - SurfBRF Pre', $
                 YTITLE='Counts per Bin', BACKGROUND=16777215, COLOR=0, $
                 XTICKLEN=1, YTICKLEN=1, XGRIDSTYLE=1, YGRIDSTYLE=1, $
                 XMARGIN=[12,3], YMARGIN=[5,3]

           ;---------------------------------------------------
           ; Require user to answer before continuing. Then 
           ; delete windows.
           ;---------------------------------------------------

           mssg = 'Press OK to continue.'
           rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)

           SafeWDELETE, 11, didit
           SafeWDELETE, 12, didit
        ENDIF

        ;--------------------------------------------------------------
        ; Write the data for this block and channel to the output.
        ;--------------------------------------------------------------

        block_offset = (MAX(TE_Block_Offsets) - TE_Block_Offsets[iblk]) / $
                       (!KON.Instr.HI_RES_PIX_CROSS / TE_Dims[0,iband])

        max_offset = MAX(TE_Block_Offsets) / $
                     (!KON.Instr.HI_RES_PIX_CROSS / TE_Dims[0,iband])

      ENDFOR  ;  end block loop

      BRF_fine_intcpts = 0
      BRF_fine_slopes  = 0
      BRF_fine_corrs   = 0

    ENDFOR  ;  end iband loop
  ENDFOR  ;  end icam loop

  ndx_bad = 0
  badndx = 0
  ndxs = 0
  ndxgood = 0

  Status = 1

END  ;  ConvertBrfData
