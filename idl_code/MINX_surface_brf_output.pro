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

;*******************************************************************
PRO HdfGlobalMetaWrite, Path, TerrPath, TerrGridName, Retval
;*******************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                          BLOCK_BEG, BLOCK_END
                         
   Retval = -1

   ;--------------------------------------------------------------------
   ; Open the output file for writing.
   ;--------------------------------------------------------------------

   fid = EOS_GD_OPEN(Path, /RDWR)
   IF (fid EQ -1) THEN BEGIN
      mssg = 'Failed to open HDFEOS file in HdfGlobalMetaWrite.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   ;-------------------------------------------------------------------
   ; Write standard parameters to the file metadata of the new file.
   ;-------------------------------------------------------------------

   status = EOS_EH_IDINFO(fid, HDFfid, sdid)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to get HDF file ids in HdfGlobalMetaWrite.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   HDF_SD_ATTRSET, sdid, 'Path_number',  PATH_NUM
   HDF_SD_ATTRSET, sdid, 'Orbit_number', ORBIT_NUM
   HDF_SD_ATTRSET, sdid, 'Number_blocks',NBLK_ORBIT
   HDF_SD_ATTRSET, sdid, 'Start_block',  BLOCK_BEG
   HDF_SD_ATTRSET, sdid, 'End block',    BLOCK_END

   ;-------------------------------------------------------------------
   ; Get SOM parameters from a terrain file and write them to the
   ; output file's global attributes.
   ;-------------------------------------------------------------------

   terr_fid = EOS_GD_OPEN(TerrPath, /READ)
   IF (terr_fid EQ -1) THEN BEGIN
      mssg = 'Failed to open Terrain file in HdfGlobalMetaWrite.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   terr_gid = EOS_EH_IDINFO(terr_fid, HDFfid, terr_sdid)
   IF (terr_gid EQ -1) THEN BEGIN
      mssg = 'Failed to attach to grid to get file metadata.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(terr_fid)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_ellipsoid.a')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_ellipsoid.a', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_ellipsoid.e2')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_ellipsoid.e2', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.aprime')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.aprime', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.eprime')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.eprime', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.gama')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.gama', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.nrev')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.nrev', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.ro')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.ro', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.i')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.i', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.P2P1')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.P2P1', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'SOM_parameters.som_orbit.lambda0')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'SOM_parameters.som_orbit.lambda0', value

   gindex = HDF_SD_ATTRFIND(terr_sdid, 'coremetadata')
   HDF_SD_ATTRINFO, terr_sdid, gindex, DATA=value
   HDF_SD_ATTRSET, sdid, 'coremetadata', value

   ;-------------------------------------------------------------------
   ; Close the terrain file and the output file.
   ;-------------------------------------------------------------------

   status = EOS_GD_CLOSE(terr_fid)
   status = EOS_GD_CLOSE(fid)

   Retval = 1

END  ; HdfGlobalMetaWrite

;*******************************************************************
PRO HdfBlockMetaCommon, SurfaceFile, BrfOutFile, Retval
;*******************************************************************
; Function copies modified PerBlockMetadataCommon from the AS surface
; file to the output BRF regression file.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON convert_common, NBLK_ORBIT

   Retval = -1

   ;--------------------------------------------------------------------
   ; Read the metadata from the AS Land file.
   ;--------------------------------------------------------------------

;   fid = HDF_OPEN(SurfaceFile, /READ)
;   IF (fid EQ -1) THEN BEGIN
;      PRINT, 'Failed to open AS Land file in HdfBlockMetaCommon.'
;      RETURN
;   ENDIF

   ;--------------------------------------------------------------------
   ; 
   ;--------------------------------------------------------------------



   ;--------------------------------------------------------------------
   ; 
   ;--------------------------------------------------------------------



   ;--------------------------------------------------------------------
   ; Write the metadata to the BRF regression file.
   ;--------------------------------------------------------------------

;   fid = EOS_GD_OPEN(BrfOutFile, /READ)
;   IF (fid EQ -1) THEN BEGIN
;      PRINT, 'Failed to open AS Land file in HdfBlockMetaCommon.'
;      RETURN
;   ENDIF


;   status = HDF_CLOSE(fid)

   Retval = 1

END  ; HdfBlockMetaCommon

;*******************************************************************
PRO HdfBlockGridCreate, Path, GridName, TerrPath, TerrGridName, $
                        NumberBlock, MaxOffset, Grid_meta, Retval
;*******************************************************************
; Create a grid in an HDFEOS file.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;--------------------------------------------------------------------
   ; Read the projection parameters for the grid from a L1B2 Terrain
   ; file with the same resolution as the grid.
   ;--------------------------------------------------------------------

   terr_fid = EOS_GD_OPEN(TerrPath, /READ)
   IF (terr_fid EQ -1) THEN BEGIN
      mssg = 'Failed to open Terrain file in HdfBlockGridCreate.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   terr_gid = EOS_GD_ATTACH(terr_fid, TerrGridName)
   IF (terr_gid EQ -1) THEN BEGIN
      mssg = 'Failed to attach to grid to get projection parameters.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(terr_fid)
      RETURN
   ENDIF

   status = EOS_GD_PROJINFO(terr_gid, projcode, zonecode, $
                            spherecode, projparm)

   status = EOS_GD_DETACH(terr_gid)

   status = EOS_GD_CLOSE(terr_fid)

   ;--------------------------------------------------------------------
   ; Open the new HDF file.
   ;--------------------------------------------------------------------

   fid = EOS_GD_OPEN(Path, /RDWR)
   IF (fid EQ -1) THEN BEGIN
      mssg = 'Failed to open output file in HdfBlockGridCreate.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   ;--------------------------------------------------------------------
   ; Create the grid.
   ;--------------------------------------------------------------------

   IF (GridName EQ !KON.Instr.HI_RES_BASE_NAME) THEN BEGIN
      cross_dim_size = LONG(!KON.Instr.HI_RES_PIX_CROSS )
      along_dim_size = LONG(!KON.Instr.HI_RES_PIX_ALONG)
      pixel_size = LONG(!KON.Instr.HI_RES_PIX_SIZE)
      gctp_upleft = DOUBLE([10584.50041,3322395.95445])
      gctp_lowright = DOUBLE([813931.10959,214162.53278])
   ENDIF ELSE BEGIN
      cross_dim_size = LONG(!KON.Instr.LO_RES_PIX_CROSS)
      along_dim_size = LONG(!KON.Instr.LO_RES_PIX_ALONG)
      pixel_size     = LONG(!KON.Instr.LO_RES_PIX_SIZE)
      gctp_upleft   = DOUBLE([10584.50041,3322395.95445])
      gctp_lowright = DOUBLE([813931.10959,214162.53278])
   ENDELSE

   max_offset = MaxOffset / (!KON.Instr.HI_RES_PIX_CROSS  / cross_dim_size)

   ;--------------------------------------------------------------------
   ; The IDL documentation for order of dimension sizes is incorrect.

   gid = EOS_GD_CREATE(fid, GridName, $
                       along_dim_size * NumberBlock, $
                       cross_dim_size + max_offset, $
                       gctp_upleft, gctp_lowright)

   IF (gid EQ -1) THEN BEGIN
      mssg = 'Failed to create grid.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   ;--------------------------------------------------------------------
   ; Copy the projection parameters into the new grid.
   ; To avoid having SOMBlock automatically added to the grid,
   ; set projparm[11] to 0 (number of blocks) !!!
   ;--------------------------------------------------------------------

   projparm[11] = 0

   status = EOS_GD_DEFPROJ(gid, projcode, zonecode, spherecode, $
			   projparm)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to write projection parameters to grid.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ENDIF

   ;--------------------------------------------------------------------
   ; Write the resolution and pixel size into the grid.
   ;--------------------------------------------------------------------

   status = EOS_GD_WRITEATTR(gid, 'Block_size.resolution_x', pixel_size)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to write Block_size.resolution_x.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ENDIF

   status = EOS_GD_WRITEATTR(gid, 'Block_size.resolution_y', pixel_size)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to write Block_size.resolution_y.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ENDIF

   status = EOS_GD_WRITEATTR(gid, 'Block_size.size_x', $
                             along_dim_size * NumberBlock)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to write Block_size.size_x.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ENDIF

   status = EOS_GD_WRITEATTR(gid, 'Block_size.size_y', $
                             cross_dim_size * 1)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to write Block_size.size_y.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   ENDIF

   ;--------------------------------------------------------------------
   ; Define dimensions used in the grid.
   ;--------------------------------------------------------------------

;   status = EOS_GD_DEFDIM(gid, "YDim", LONG(cross_dim_size + MaxOffset))
;   IF (status EQ -1) THEN PRINT, 'Failed to write YDim to grid.'
;   status = EOS_GD_DEFDIM(gid, "XDim", LONG(along_dim_size * NumberBlock))
;   IF (status EQ -1) THEN PRINT, "Failed to write XDim to grid.'

   ;--------------------------------------------------------------------
   ; Detach from the grid.
   ;--------------------------------------------------------------------

   status = EOS_GD_DETACH(gid)

   ;--------------------------------------------------------------------
   ; Write the grid metadata.
   ;--------------------------------------------------------------------

;   status = HdfBlockFidGridMetaWrite(fid, GridName, Grid_meta)

   ;--------------------------------------------------------------------
   ; Close the file.
   ;--------------------------------------------------------------------

   status = EOS_GD_CLOSE(fid)

   Retval = 1

END  ; HdfBlockGridCreate

;*******************************************************************
PRO HdfBlockGctpOffsetDefine, Path, GridName, NumberBlock, $
                              GctpOffset
;*******************************************************************
; 
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;--------------------------------------------------------------------
   ; 
   ;--------------------------------------------------------------------


   Retval = 1

END  ; HdfBlockGctpOffsetDefine

;*******************************************************************
PRO HdfBlockGridFieldCreate, Path, GridName, FieldName, $
                             NumberBlock, DefaultFill, Retval
;*******************************************************************
; Create a field in an HDFEOS grid.
;-----------------------------------------------------------------------
  
COMPILE_OPT IDL2, LOGICAL_PREDICATE

   Retval = -1

   ;--------------------------------------------------------------------
   ; Open the file and attach to the grid.
   ;--------------------------------------------------------------------

   fid = EOS_GD_OPEN(Path, /RDWR)
   IF (fid EQ -1) THEN BEGIN
      mssg = 'Failed to open HDFEOS file to create field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF

   gid = EOS_GD_ATTACH(fid, GridName)
   IF (gid EQ -1) THEN BEGIN
      mssg = 'Failed to attach to grid to create field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   ;--------------------------------------------------------------------
   ; Define the default fill value, this must be done before tiling
   ; and compression are defined.
   ;--------------------------------------------------------------------

;   status = EOS_GD_SETFILLVALUE(gid, FieldName, DefaultFill) 
;   IF (status EQ -1) THEN BEGIN
;      mssg = 'Failed to set fill value for field ' + FieldName
;      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;      status = EOS_GD_DETACH(gid)
;      status = EOS_GD_CLOSE(fid)
;      RETURN
;   ENDIF

   ;--------------------------------------------------------------------
   ; Define the tiling and compression for the field to create.
   ; IDL documentation about order of sizes is wrong.
   ;--------------------------------------------------------------------

   status = EOS_GD_GRIDINFO(gid, along_dim_size, cross_dim_size, $
                            upleft, lowright)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to get grid info for field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_DETACH(gid)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   IF (GridName EQ !KON.Instr.HI_RES_BASE_NAME) THEN BEGIN
      along_dim_size = LONG(!KON.Instr.HI_RES_PIX_ALONG)
   ENDIF ELSE BEGIN
      along_dim_size = LONG(!KON.Instr.LO_RES_PIX_ALONG)
   ENDELSE

   status = EOS_GD_DEFCOMP(gid, 4, [5])
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to define compression for field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_DETACH(gid)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   tile_dims = [cross_dim_size, along_dim_size]
   status = EOS_GD_DEFTILE(gid, 1, 2, tile_dims)
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to define tiling for field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_DETACH(gid)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   ;--------------------------------------------------------------------
   ; Define the field.
   ;--------------------------------------------------------------------

   status = EOS_GD_DEFFIELD(gid, FieldName, "YDim,XDim", LONG(5))
   IF (status EQ -1) THEN BEGIN
      mssg = 'Failed to define field ' + FieldName
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      status = EOS_GD_DETACH(gid)
      status = EOS_GD_CLOSE(fid)
      RETURN
   ENDIF

   ;--------------------------------------------------------------------
   ; Detach from the grid and close the file.
   ;--------------------------------------------------------------------

   status = EOS_GD_DETACH(gid)

   status = EOS_GD_CLOSE(fid)

   Retval = 1

END  ; HdfBlockGridFieldCreate

;*******************************************************************
PRO CreateOutputFile, FileType, OutputDir, ChannelMask, ASOffsets, $
                      TerrainFiles, AsSurfaceFile, BrfOutputFile, $
                      RegressSize, DoSmooth, FilterWidth, Status
;*******************************************************************
; Create the HDFEOS, HDF or ASCII output file.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                          BLOCK_BEG, BLOCK_END

   Status = -1

   TerrainGrids = !KON.Instr.BAND_NAMES + 'Band'

   ;--------------------------------------------------------------------
   ; Build output file name.
   ;--------------------------------------------------------------------

   pathstr = STRTRIM(STRING(PATH_NUM),2)
   IF (STRLEN(pathstr) EQ 1) THEN pathstr = '0' + pathstr
   IF (STRLEN(pathstr) EQ 2) THEN pathstr = '0' + pathstr
   pathstr = 'P' + pathstr

   orbitstr = STRTRIM(STRING(ORBIT_NUM),2)
   IF (STRLEN(orbitstr) EQ 3) THEN orbitstr = '0' + orbitstr
   IF (STRLEN(orbitstr) EQ 4) THEN orbitstr = '0' + orbitstr
   IF (STRLEN(orbitstr) EQ 5) THEN orbitstr = '0' + orbitstr
   orbitstr = 'O' + orbitstr

   jdate = SYSTIME()
   kdate = STRMID(jdate,4,3) + STRMID(jdate,8,2) + '.' + $
           STRMID(jdate,22,2) + '.' + STRMID(jdate,11,2) + '.' + $
           STRMID(jdate,14,2)

   filename = 'MINX_LAND_BRF_' + pathstr + '_' + orbitstr + $
              ((FileType EQ 3) ? '.txt' : '.hdf')
   BrfOutputFile = OutputDir + filename

   ;--------------------------------------------------------------------
   ; Let the user change the file path and name.
   ;--------------------------------------------------------------------

   filter_name = 'MINX_LAND_BRF_*' + $
                 ((FileType EQ 3) ? '.txt' : '.hdf')

   ftype = (FileType EQ 1) ? 'HDF-EOS' : $
          ((FileType EQ 2) ? 'HDF' : 'ASCII')
   title = 'Specify ' + ftype + ' File to Contain Land BRF Output'

   outfile = DIALOG_PICKFILE(PATH=OutputDir, FILE=filename, $
                      TITLE=title, FILTER=filter_name)

   IF (outfile EQ '') THEN BEGIN
      mssg = 'You have chosen no file.  Quitting.'
      rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
      RETURN
   ENDIF
  
   BrfOutputFile = outfile

   ;--------------------------------------------------------------------
   ; If requested, set up HDFEOS file.
   ;--------------------------------------------------------------------

   IF (FileType EQ 1) THEN BEGIN

      ;-----------------------------------------------------------------
      ; Create HDFEOS file and close it again.  First test if it
      ; already exists.
      ;-----------------------------------------------------------------

      IF (FILE_TEST(BrfOutputFile) EQ 1) THEN BEGIN
         retval = DIALOG_MESSAGE( $
                  ['Do you want to overwrite existing file:', $
                   BrfOutputFile], /QUESTION, /CENTER)
         IF (STRUPCASE(retval) EQ 'YES') THEN BEGIN
            FILE_DELETE, BrfOutputFile   ; hack for EOS_GD_OPEN
            WAIT, 2
         ENDIF ELSE BEGIN
            mssg = 'Quitting MINX.'
            rval = DIALOG_MESSAGE(mssg, /INFORMATION, /CENTER)
            RETURN
         ENDELSE
     ENDIF

     fid = EOS_GD_OPEN(BrfOutputFile, /CREATE)
     IF (fid EQ -1) THEN BEGIN
        mssg = 'Failed to create new HDFEOS file.'
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
     ENDIF
     status = EOS_GD_CLOSE(fid)
     IF (status EQ -1) THEN BEGIN
        mssg = 'Failed to close newly created HDFEOS file.'
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
     ENDIF
  
     ;-----------------------------------------------------------------
     ; Determine if there are any hi res and any lo res channels
     ; (fields). Also count the number of bands and cameras for
     ; each resolution.
     ;-----------------------------------------------------------------

     lores_exists = 0
     ndx = WHERE(ChannelMask EQ !KON.Instr.LO_RES_MASK, numres)
     IF (numres GT 0) THEN BEGIN
        lores_exists = 1
        lores_cam = (ARRAY_INDICES(ChannelMask, ndx[0]))[0]
        lores_band = (ARRAY_INDICES(ChannelMask, ndx[0]))[1]
     ENDIF

     hires_exists = 0
     ndx = WHERE(ChannelMask EQ !KON.Instr.HI_RES_MASK, numres)
     IF (numres GT 0) THEN BEGIN
        hires_exists = 1
        hires_cam = (ARRAY_INDICES(ChannelMask, ndx[0]))[0]
        hires_band = (ARRAY_INDICES(ChannelMask, ndx[0]))[1]
     ENDIF

     IF (lores_exists) THEN BEGIN 
        res_cam = lores_cam
        res_band = lores_band
     ENDIF ELSE IF (hires_exists) THEN BEGIN 
        res_cam = hires_cam
        res_band = hires_band
     ENDIF ELSE BEGIN
	mssg = 'You have chosen no channels to process.  Quitting.'
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
     ENDELSE

     ;-----------------------------------------------------------------
     ; Write global metadata to the new file.
     ;-----------------------------------------------------------------

     HdfGlobalMetaWrite, BrfOutputFile, TerrainFiles[res_cam], $
                         TerrainGrids[res_band], status

     ;-----------------------------------------------------------------
     ; Create the grids.
     ;-----------------------------------------------------------------

grid_meta = 1

     IF (lores_exists) THEN BEGIN
        HdfBlockGridCreate, BrfOutputFile, !KON.Instr.LO_RES_BASE_NAME, $
                            TerrainFiles[lores_cam], $
                            TerrainGrids[lores_band], $
                            NBLK_ORBIT, MAX(ASOffsets), $
                            grid_meta, status
        IF (status EQ -1) THEN BEGIN
           mssg = 'Failed to create low resolution grid.'
           rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
           RETURN
        ENDIF
     ENDIF

     IF (hires_exists) THEN BEGIN
        HdfBlockGridCreate, BrfOutputFile, !KON.Instr.HI_RES_BASE_NAME, $
                            TerrainFiles[hires_cam], $
                            TerrainGrids[hires_band], $
                            NBLK_ORBIT, MAX(ASOffsets), $
                            grid_meta, status
        IF (status EQ -1) THEN BEGIN
           mssg = 'Failed to create high resolution grid.'
           rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
           RETURN
        ENDIF
     ENDIF

     ;-----------------------------------------------------------------
     ; Read the gctp offset arrays from terrain and write them to
     ; the grids.
     ;-----------------------------------------------------------------

;     IF (lores_exists) THEN BEGIN
;        FOR i = 0, agp_meta.common.number_block-1 DO BEGIN
;           gctp_offset[i] = agp_gctp_offset[i] * $
;              (float)agp_grid_meta_stnd.common.block_size.resolution_y / $
;              (float)agp_grid_meta_lores.common.block_size.resolution_y
;        ENDFOR
;        HdfBlockGctpOffsetDefine, BrfOutputFile, $
;                                  !KON.Instr.LO_RES_BASE_NAME, $
;                                  agp_meta.common.number_block-1, $
;                                  gctp_offset, status
;        IF (status EQ -1) THEN BEGIN
;           mssg = 'Failed to define gctp offset for low res grid.'
;           rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;           RETURN
;        ENDIF
;     ENDIF

;     IF (hires_exists) THEN BEGIN
;        FOR i = 0, agp_meta.common.number_block-1 DO BEGIN
;           gctp_offset[i] = agp_gctp_offset[i] * $
;              (float)agp_grid_meta_stnd.common.block_size.resolution_y / $
;              (float)agp_grid_meta_hires.common.block_size.resolution_y
;        ENDFOR
;        HdfBlockGctpOffsetDefine, BrfOutputFile, $
;                                  !KON.Instr.HI_RES_BASE_NAME, $
;                                  agp_meta.common.number_block-1, $
;                                  gctp_offset, status
;        IF (status EQ -1) THEN BEGIN
;           mssg = 'Failed to define gctp offset for high res grid.'
;           rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
;           RETURN
;        ENDIF
;     ENDIF

     ;-----------------------------------------------------------------
     ; Define the grid fields.
     ;-----------------------------------------------------------------

     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN

           IF (ChannelMask[icam,iband] EQ $
               !KON.Instr.LO_RES_MASK) THEN BEGIN
              field_name = 'CAM_' + !KON.Instr.CAM_NAMES[icam] + $
                           '_BAND_' + !KON.Instr.BAND_NAMES[iband]
              HdfBlockGridFieldCreate, BrfOutputFile, $
                                       !KON.Instr.LO_RES_BASE_NAME, $
                                       field_name, NBLK_ORBIT, $
                                       !KON.Misc.BADVALUE_REAL, status
              IF (status EQ -1) THEN BEGIN
                 mssg = 'Failed to create grid field ' + field_name
                 rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
                 RETURN
              ENDIF
           ENDIF

           IF (ChannelMask[icam,iband] EQ $
               !KON.Instr.HI_RES_MASK) THEN BEGIN
              field_name = 'CAM_' + !KON.Instr.CAM_NAMES[icam] + $
                           '_BAND_' + !KON.Instr.BAND_NAMES[iband]
              HdfBlockGridFieldCreate, BrfOutputFile, $
                                       !KON.Instr.HI_RES_BASE_NAME, $
                                       field_name, NBLK_ORBIT, $
                                       !KON.Misc.BADVALUE_REAL, status
              IF (status EQ -1) THEN BEGIN
                 mssg = 'Failed to create grid field ' + field_name
                 rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
                 RETURN
              ENDIF
           ENDIF

        ENDFOR
     ENDFOR

     ;-----------------------------------------------------------------
     ; Create the common block metadata.
     ;-----------------------------------------------------------------

     HdfBlockMetaCommon, AsSurfaceFile, BrfOutputFile, retval

     ;-----------------------------------------------------------------
     ; Write annotation text to file.
     ;-----------------------------------------------------------------

     hfile = HDF_OPEN(BrfOutputFile, /RDWR)

     IF (hfile LT 0) THEN BEGIN
        retval = DIALOG_MESSAGE( $
                 ['Could not open file:', BrfOutputFile, $
                  'to write annotation text.  Quitting.'], /ERROR, /CENTER)
        RETURN
     ENDIF

     annotation = 'Creation date and time: ' + jdate + !KON.Misc.NewLine + $
                  'Input Surface file: ' + AsSurfaceFile + !KON.Misc.NewLine

     ndxgood = WHERE(TerrainFiles NE '', numgood)

     IF (numgood GT 0) THEN BEGIN
        FOR icam = 0, numgood-1 DO BEGIN
           camstr = STRTRIM(STRING(icam),2)
           annotation += 'Input Terrain file ' + camstr + ': ' + $
                         TerrainFiles[ndxgood[icam]] + !KON.Misc.NewLine
        ENDFOR
     ENDIF

     HDF_DFAN_ADDFID, BrfOutputFile, annotation

     HDF_CLOSE, hfile

  ENDIF

  ;--------------------------------------------------------------------
  ; If requested, set up HDF file.
  ;--------------------------------------------------------------------

  IF (FileType EQ 2) THEN BEGIN

     ;-----------------------------------------------------------------
     ; Create HDF file.
     ;-----------------------------------------------------------------

     hfile = HDF_OPEN(BrfOutputFile, /CREATE, /WRITE)

     IF (hfile LT 0) THEN BEGIN
        retval = DIALOG_MESSAGE( $
                 ['A new file named:', BrfOutputFile, $
                  'could not be created.  Quitting.'], /ERROR, /CENTER)
        RETURN
     ENDIF

     ;-----------------------------------------------------------------
     ; Write annotation text to file.
     ;-----------------------------------------------------------------

     annotation = 'Creation date and time: ' + jdate
     HDF_DFAN_ADDFID, BrfOutputFile, annotation

     annotation = 'Input Surface file: ' + AsSurfaceFile
     HDF_DFAN_ADDFID, BrfOutputFile, annotation

     ndxgood = WHERE(TerrainFiles NE '', numgood)

     IF (numgood GT 0) THEN BEGIN
        FOR icam = 0, numgood-1 DO BEGIN
           camstr = STRTRIM(STRING(icam),2)
           annotation = 'Input Terrain file ' + camstr + ': ' + $
                        TerrainFiles[ndxgood[icam]]
           HDF_DFAN_ADDFID, BrfOutputFile, annotation
        ENDFOR
     ENDIF

     ;-----------------------------------------------------------------
     ; Close the file.
     ;-----------------------------------------------------------------

     HDF_CLOSE, hfile

     ;-----------------------------------------------------------------
     ; Create hi-res scientific datasets and attributes for those
     ; channels that are hi-res.  Assign the datasets to a hi-res
     ; VGroup. Add protection in case HDF file is damaged!
     ;-----------------------------------------------------------------

     sd_id = HDF_SD_START(BrfOutputFile, /RDWR)

     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
           IF (ChannelMask[icam,iband] EQ $
               !KON.Instr.HI_RES_MASK) THEN BEGIN

              field_name = !KON.Instr.HI_RES_BASE_NAME + '_' + $
                           !KON.Instr.CAM_NAMES[icam] + $
                           '_' + !KON.Instr.BAND_NAMES[iband]
              dims = [!KON.Instr.HI_RES_PIX_CROSS, $
                      !KON.Instr.HI_RES_PIX_ALONG, $
                      NBLK_ORBIT]

              sds_id = HDF_SD_CREATE(sd_id, field_name, dims, /FLOAT)

              dim_id = HDF_SD_DIMGETID(sds_id, 0)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='YDim_275m'
              dim_id = HDF_SD_DIMGETID(sds_id, 1)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='XDim_275m'
              dim_id = HDF_SD_DIMGETID(sds_id, 2)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='SOMBlockDim_275m'

              HDF_SD_ATTRSET, sds_id, 'fill value', $
                              [!KON.Misc.BADVALUE_REAL], 1, /FLOAT

              HDF_SD_ENDACCESS, sds_id

           ENDIF
        ENDFOR
     ENDFOR

;     Result = HDF_VG_ATTACH(FileHandle, VGroup_id [, /READ] [, /WRITE] )
;     HDF_VG_SETINFO, VGroup [, CLASS=string] [, NAME=string]
;     HDF_VG_INSERT, VGroup, VData(or Vgroup)[, POSITION=variable]
;     HDF_VG_DETACH, VGroup

     ;-----------------------------------------------------------------
     ; Create lo-res scientific datasets and attributes for those
     ; channels that are lo-res.  Assign the datasets to a lo-res
     ; VGroup.
     ;-----------------------------------------------------------------

     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
           IF (ChannelMask[icam,iband] EQ $
               !KON.Instr.LO_RES_MASK) THEN BEGIN

              field_name = !KON.Instr.LO_RES_BASE_NAME + '_' + $
                           !KON.Instr.CAM_NAMES[icam] + $
                           '_' + !KON.Instr.BAND_NAMES[iband]
              dims = [!KON.Instr.LO_RES_PIX_CROSS, $
                      !KON.Instr.LO_RES_PIX_ALONG, NBLK_ORBIT]

              sds_id = HDF_SD_CREATE(sd_id, field_name, dims, /FLOAT)

              dim_id = HDF_SD_DIMGETID(sds_id, 0)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='YDim_1100m'
              dim_id = HDF_SD_DIMGETID(sds_id, 1)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='XDim_1100m'
              dim_id = HDF_SD_DIMGETID(sds_id, 2)
              HDF_SD_DIMSET, dim_id, /BW_INCOMP, NAME='SOMBlockDim_1100m'

              HDF_SD_ATTRSET, sds_id, 'fill value', $
                              [!KON.Misc.BADVALUE_REAL], 1, /FLOAT

              HDF_SD_ENDACCESS, sds_id

           ENDIF
        ENDFOR
     ENDFOR

     HDF_SD_END, sd_id

  ENDIF

  ;--------------------------------------------------------------------
  ; If requested, create ASCII file.
  ;--------------------------------------------------------------------

  IF (FileType EQ 3) THEN BEGIN

     OPENW, unit, BrfOutputFile, /GET_LUN

     ;-----------------------------------------------------------------
     ; Write the file header information.
     ;-----------------------------------------------------------------

     PRINTF, unit, 'Land BRF data computed with MINX by ' + $
                'Regression method from MISR_AM1_AS_LAND and ' + $
                'MISR_AM1_GRP_TERRAIN products'
     PRINTF, unit, '--------------------------------------------' + $
                '--------------------------------------------' + $
                '-------------------------'

     PRINTF, unit, 'Path Number:  ' + STRTRIM(STRING(PATH_NUM),2)
     PRINTF, unit, 'Orbit Number: ' + STRTRIM(STRING(ORBIT_NUM),2)
     PRINTF, unit, 'First Block:  ' + STRTRIM(STRING(BLOCK_BEG),2)
     PRINTF, unit, 'Last Block:   ' + STRTRIM(STRING(BLOCK_END),2)
     julian = SYSTIME(/JULIAN)
     CALDAT, julian, month, day, year
     PRINTF, unit, 'Date file generated: ' + $
                STRTRIM(STRING(month),2) + '/' + $
                STRTRIM(STRING(day),2) + '/' + $
                STRTRIM(STRING(year),2)

     PRINTF, unit, 'Meters on edges of regression squares: ' + $
                STRTRIM(STRING(RegressSize),2)
     filter_width = FilterWidth
     IF (DoSmooth EQ 0) THEN filter_width = 0
     PRINTF, unit, 'Pixels on edges of smoothing filter:   ' + $
                STRTRIM(STRING(filter_width),2)
     PRINTF, unit, 'Bad value indicator: ' + $
                STRTRIM(STRING(!KON.Misc.BADVALUE_ASCII),2)

     PRINTF, unit, 'MISR channels present in file at 1.1 km resolution:'
     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        PRINTF, unit, FORMAT='($,A)', '  Camera ' + $
                                   !KON.Instr.CAM_NAMES[icam] + ': '
        FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
           IF (ChannelMask[icam,iband] EQ !KON.Instr.LO_RES_MASK) THEN $
              PRINTF, unit, FORMAT='($,A)', !KON.Instr.BAND_NAMES[iband] + ' '
        ENDFOR
        PRINTF, unit, ''  ; carriage return
     ENDFOR

     PRINTF, unit, 'MISR channels present in file at 275 m resolution:'
     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        PRINTF, unit, FORMAT='($,A)', '  Camera ' + $
                                   !KON.Instr.CAM_NAMES[icam] + ': '
        FOR iband = 0, !KON.Instr.NBAND-1 DO BEGIN
           IF (ChannelMask[icam,iband] EQ !KON.Instr.HI_RES_MASK) THEN $
              PRINTF, unit, FORMAT='($,A)', !KON.Instr.BAND_NAMES[iband] + ' '
        ENDFOR
        PRINTF, unit, ''  ; carriage return
     ENDFOR
     
     PRINTF, unit, 'Input MISR_AM1_AS_LAND File:'
     PRINTF, unit, '  ' + AsSurfaceFile
     PRINTF, unit, 'Input MISR_AM1_GRP_TERRAIN Files:'
     FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
        IF (TerrainFiles[icam] EQ '') THEN BEGIN
           PRINTF, unit, '  No ' + !KON.Instr.CAM_NAMES[icam] + $
                   ' camera GRP_TERRAIN file used'
        ENDIF ELSE BEGIN
           PRINTF, unit, '  ' + TerrainFiles[icam]
        ENDELSE
     ENDFOR
     
     ;-----------------------------------------------------------------
     ; Close the file and return.
     ;-----------------------------------------------------------------
     
     FREE_LUN, unit

  ENDIF

  ndx = 0
  ndxgood = 0

  Status = 1

END  ;  CreateOutputFile

;*******************************************************************
PRO WriteOutputToFile, FileType, BrfOutputFile, ChannelMask, CamNum, $
                       BandNum, BlockNum, BlockData, BlkOffset, $
                       MaxOffset, Status
;*******************************************************************
; Write one block of data for one channel to the HDFEOS, HDF or
; ASCII output file.
;-----------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

  COMMON convert_common, NBLK_ORBIT, PATH_NUM, ORBIT_NUM, $
                         BLOCK_BEG, BLOCK_END                       
  Status = -1

  ;--------------------------------------------------------------------
  ; If requested, write data to HDFEOS file.
  ;--------------------------------------------------------------------

  IF (FileType EQ 1) THEN BEGIN

     ;-----------------------------------------------------------------
     ; Determine base name for the dataset name and dimension data.
     ;-----------------------------------------------------------------

     IF (ChannelMask[CamNum,BandNum] EQ $
         !KON.Instr.HI_RES_MASK) THEN BEGIN
        base_dsname = !KON.Instr.HI_RES_BASE_NAME
        count1 = [!KON.Instr.HI_RES_PIX_CROSS + MaxOffset, $
                  !KON.Instr.HI_RES_PIX_ALONG]
	begpt1 = [0, (BlockNum-BLOCK_BEG+1) * $
                  !KON.Instr.HI_RES_PIX_ALONG]
        count2 = [!KON.Instr.HI_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_ALONG]
	begpt2 = [BlkOffset, (BlockNum-BLOCK_BEG+1) * $
                  !KON.Instr.HI_RES_PIX_ALONG]
     ENDIF
     IF (ChannelMask[CamNum,BandNum] EQ !KON.Instr.LO_RES_MASK) THEN BEGIN
        base_dsname = !KON.Instr.LO_RES_BASE_NAME
        count1 = [!KON.Instr.LO_RES_PIX_CROSS + MaxOffset, $
                  !KON.Instr.LO_RES_PIX_ALONG]
	begpt1 = [0, (BlockNum-BLOCK_BEG+1) * $
                  !KON.Instr.LO_RES_PIX_ALONG]
        count2 = [!KON.Instr.LO_RES_PIX_CROSS, $
                  !KON.Instr.LO_RES_PIX_ALONG]
	begpt2 = [BlkOffset, (BlockNum-BLOCK_BEG+1) * $
                  !KON.Instr.LO_RES_PIX_ALONG]
     ENDIF

     fid = EOS_GD_OPEN(BrfOutputFile, /RDWR)
     IF (fid EQ -1) THEN BEGIN
        mssg = 'Failed to open HDFEOS file to write block ' + $
               STRTRIM(STRING(BlockNum+1),2)
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        RETURN
     ENDIF

     gid = EOS_GD_ATTACH(fid, base_dsname)
     IF (gid EQ -1) THEN BEGIN
        mssg = 'Failed to attach to grid to write block ' + $
               STRTRIM(STRING(BlockNum+1),2)
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        status = EOS_GD_CLOSE(fid)
        RETURN
     ENDIF

     field_name = 'CAM_' + !KON.Instr.CAM_NAMES[CamNum] + $
                  '_BAND_' + !KON.Instr.BAND_NAMES[BandNum]

     ;-----------------------------------------------------------------
     ; First fill entire block area including offset space with
     ; bad value indicators, then write just block data in.
     ;-----------------------------------------------------------------

     blk_fill = FLTARR(count1[0], count1[1]) + !KON.Misc.BADVALUE_FILL

     status = EOS_GD_WRITEFIELD(gid, field_name, blk_fill, $
                                START=begpt1, EDGE=count1)

     blk_fill = 0

     IF (status EQ -1) THEN BEGIN
        mssg = 'Failed to write fill for block ' + $
               STRTRIM(STRING(BlockNum+1),2)
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        status = EOS_GD_CLOSE(fid)
        RETURN
     ENDIF

     status = EOS_GD_WRITEFIELD(gid, field_name, BlockData, $
                                START=begpt2, EDGE=count2)

     IF (status EQ -1) THEN BEGIN
        mssg = 'Failed to write data for block ' + $
               STRTRIM(STRING(BlockNum+1),2)
        rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
        status = EOS_GD_CLOSE(fid)
        RETURN
     ENDIF

     status = EOS_GD_DETACH(gid)

     status = EOS_GD_CLOSE(fid)

  ENDIF

  ;--------------------------------------------------------------------
  ; If requested, write data to HDF file.
  ;--------------------------------------------------------------------

  IF (FileType EQ 2) THEN BEGIN

     ;-----------------------------------------------------------------
     ; Determine base name for the dataset name and dimension data.
     ; Add protection in case HDF file is damaged!
     ;-----------------------------------------------------------------

     IF (ChannelMask[CamNum,BandNum] EQ !KON.Instr.HI_RES_MASK) THEN BEGIN
        base_dsname = !KON.Instr.HI_RES_BASE_NAME
        count = [!KON.Instr.HI_RES_PIX_CROSS, $
                 !KON.Instr.HI_RES_PIX_ALONG, 1]
     ENDIF
     IF (ChannelMask[CamNum,BandNum] EQ !KON.Instr.LO_RES_MASK) THEN BEGIN
        base_dsname = !KON.Instr.LO_RES_BASE_NAME
        count = [!KON.Instr.LO_RES_PIX_CROSS, $
                 !KON.Instr.LO_RES_PIX_ALONG, 1]
     ENDIF

     sdfile_id = HDF_SD_START(BrfOutputFile, /RDWR)
     IF (sdfile_id LE 0) THEN BEGIN
        retval = DIALOG_MESSAGE( $
                 ['File ' + BrfOutputFile, $
                  'could not be opened.  Quitting.'], /ERROR, /CENTER)
        RETURN
     ENDIF

     sd_dsname = base_dsname + '_' + !KON.Instr.CAM_NAMES[CamNum] + '_' + $
                 !KON.Instr.BAND_NAMES[BandNum]

     sd_ndx = HDF_SD_NAMETOINDEX(sdfile_id, sd_dsname)
     IF (sd_ndx LT 0) THEN BEGIN
        retval = DIALOG_MESSAGE( $
                 ['SDS ' + sd_dsname + ' could not be opened.', $
                  'Skipping this channel.'], /ERROR, /CENTER)
        HDF_SD_END, sdfile_id
        RETURN
     ENDIF
  
     sd_id = HDF_SD_SELECT(sdfile_id, sd_ndx)
     IF (sd_id LE 0) THEN BEGIN
        retval = DIALOG_MESSAGE( $
                 ['SDS ' + sd_dsname + ' could not be selected.', $
                  'Skipping this channel.'], /ERROR, /CENTER)
        HDF_SD_END, sdfile_id
        RETURN
     ENDIF

     HDF_SD_ADDDATA, sd_id, BlockData, COUNT=count, START=[0,0,0]
     HDF_SD_ENDACCESS, sd_id
     
     HDF_SD_END, sdfile_id

  ENDIF

  ;--------------------------------------------------------------------
  ; If requested, write data to ASCII file.
  ;--------------------------------------------------------------------

  IF (FileType EQ 3) THEN BEGIN

     OPENU, unit1, BrfOutputFile, /APPEND, /GET_LUN

     ResCross  = [0, !KON.Instr.HI_RES_PIX_CROSS, $
                     !KON.Instr.LO_RES_PIX_CROSS]
     ResAlong  = [0, !KON.Instr.HI_RES_PIX_ALONG, $
                     !KON.Instr.LO_RES_PIX_ALONG]
     ResString = [0, !KON.Instr.HI_RES_PIX_SIZE, $
                     !KON.Instr.LO_RES_PIX_SIZE]
     FmtString = '(18F8.3)'
     FmtDescr  = ' per record; samples in line 1, ' + $
                              'samples in line 2, ...'

     ;-----------------------------------------------------------------
     ; Write the dash separator.
     ;-----------------------------------------------------------------
     
     PRINTF, unit1, '--------------------------------------' + $
                    '--------------------------------------' + $
                    '-------------------------------------'

     ;-----------------------------------------------------------------
     ; Write block header information.
     ;-----------------------------------------------------------------

     ndx_val = ChannelMask[CamNum,BandNum]

     PRINTF, unit1, 'Camera name:      ' + !KON.Instr.CAM_NAMES[CamNum]
     PRINTF, unit1, 'Band name:        ' + !KON.Instr.BAND_NAMES[BandNum]
     PRINTF, unit1, 'Block number:     ' + STRTRIM(STRING(BlockNum+1),2)
     PRINTF, unit1, 'Samples per line: ' + $
                     STRTRIM(STRING(ResCross[ndx_val]),2)
     PRINTF, unit1, 'Lines per block:  ' + $
                     STRTRIM(STRING(ResAlong[ndx_val]),2)
     PRINTF, unit1, 'Pixel size (m):   ' + $
                     STRTRIM(STRING(ResString[ndx_val]),2)
     PRINTF, unit1, 'Record format:    ' + FmtString + FmtDescr

     ;-----------------------------------------------------------------
     ; Write block data.
     ;-----------------------------------------------------------------

     nline = ResAlong[ndx_val]

     FOR iline = 0, nline-1 DO BEGIN
        dataline = BlockData[*,iline]
        ndxbad = WHERE(dataline EQ !KON.Misc.BADVALUE_REAL, numbad)
        IF (numbad GT 0) THEN dataline[ndxbad] = !KON.Misc.BADVALUE_ASCII
        PRINTF, unit1, FORMAT=FmtString, dataline
     ENDFOR     

     ndxbad = 0
     FREE_LUN, unit1

  ENDIF

END  ;  WriteOutputToFile
