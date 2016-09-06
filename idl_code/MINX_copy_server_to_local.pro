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

;****************************************************************************
PRO BlockRangeTest, Template, BlkRange, OutPath
;****************************************************************************
; Determine whether the passed file contains a block range that
; satisfies the users criteria.
; The search is successful if [the entire orbit was requested AND
; the file contains the entire orbit (no .b or .subset component)]
; OR [a subsetted orbit was requested AND the file contains blocks
; that include those requested].
; Reject all files with the '.subset' designation.
;-------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;-------------------------------------------------------------------------
; Get all the filenames that satisfy the template and loop over them.
; Consider FIRSTLOOK orbits if user requested it.
;-------------------------------------------------------------------------

out_path = ''
filelist = ['']
filelist = FILE_SEARCH(Template)
nfile = N_ELEMENTS(filelist)

;-------------------------------------------------------------------------
; Sort the filenames by length, shortest first. This puts the whole
; swath name first so it can be superceded by subsetted files if found.
;-------------------------------------------------------------------------

namelen = INTARR(nfile)
FOR ifile=0,nfile-1 DO BEGIN
   namelen[ifile] = STRLEN(filelist[ifile])
ENDFOR

sortndx = SORT(namelen)
filelist = filelist[sortndx]

req_blks = FIX(STRSPLIT(BlkRange, '.b-', /EXTRACT))

IF (filelist[0] NE '') THEN BEGIN

   FOR ifile=0,nfile-1 DO BEGIN

      ;-------------------------------------------------------------------
      ; Find the string position of certain critical components of
      ; filename.
      ;-------------------------------------------------------------------

      ipos1 = STRPOS(filelist[ifile], '.b',   /REVERSE_SEARCH)
      ipos2 = STRPOS(filelist[ifile], '.hdf', /REVERSE_SEARCH)
      ipos3 = STRPOS(filelist[ifile], '.subset',/REVERSE_SEARCH)

      ;-------------------------------------------------------------------
      ; Determine if the file satisfies the block range constraints.
      ;-------------------------------------------------------------------

      IF (BlkRange EQ '') THEN BEGIN   ; entire orbit requested

         IF (ipos1 LT 0 AND ipos3 LT 0) THEN BEGIN
            IF (out_path EQ '') THEN BEGIN
               out_path = filelist[ifile]
               BREAK
            ENDIF
         ENDIF

      ENDIF ELSE BEGIN   ; subsetted orbit requested

         IF (ipos1 GE 0 AND ipos3 LT 0) THEN BEGIN   ; file has .b in name

            fileblk = STRMID(filelist[ifile], ipos1+2, ipos2-ipos1-2)
            file_blks = FIX(STRSPLIT(fileblk, '-', /EXTRACT))
            IF (N_ELEMENTS(file_blks) NE 2) THEN CONTINUE

            IF (file_blks[0] LE req_blks[0] AND $
                file_blks[1] GE req_blks[1]) THEN BEGIN
               out_path = filelist[ifile]
               IF (file_blks[0] EQ req_blks[0] AND $
                   file_blks[1] EQ req_blks[1]) THEN BREAK
            ENDIF

         ENDIF ELSE BEGIN

            IF (ipos1 LT 0 AND ipos3 LT 0) THEN BEGIN   ; file is whole swath
               out_path = filelist[ifile]
            ENDIF
         ENDELSE

      ENDELSE

   ENDFOR

ENDIF

OutPath = out_path

END  ;  BlockRangeTest

;****************************************************************
PRO CopyFromServerToLocal, DefaultProjDir
;****************************************************************
; Program is used before running the MINX plume analysis program
; to copy MISR products needed by MINX from a server to the local
; machine for a significant improvement in loading speed or to
; load data onto a laptop or external hard drive. User required
; data types and versions are read from an ASCII file.
;---------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;---------------------------------------------------------------------
; Declare constants.
;---------------------------------------------------------------------

cams = STRUPCASE(!KON.Instr.CAM_NAMES)

type_names = ['GRP_ELLIPSOID','GRP_TERRAIN','GRP_RCCM','AGP','GP_GMP', $
              'TC_STEREO','TC_CLOUD','TC_CLASSIFIERS','TC_ALBEDO', $
              'AS_AEROSOL','AS_LAND']

ell_begname = 'MISR_AM1_GRP_ELLIPSOID_GM_P'
ter_begname = 'MISR_AM1_GRP_TERRAIN_GM_P'
rcc_begname = 'MISR_AM1_GRP_RCCM_GM_P'
agp_begname = 'MISR_AM1_AGP_P'
gmp_begname = 'MISR_AM1_GP_GMP_P'
aer_begname = 'MISR_AM1_AS_AEROSOL_P'
lan_begname = 'MISR_AM1_AS_LAND_P'
ste_begname = 'MISR_AM1_TC_STEREO_P'
cld_begname = 'MISR_AM1_TC_CLOUD_P'
cls_begname = 'MISR_AM1_TC_CLASSIFIERS_P'
alb_begname = 'MISR_AM1_TC_ALBEDO_P'

mssg = ['An error was encountered reading the control file.',   $
        'Correct the problem so the format is as follows:','',  $
        '   GRP_ELLIPSOID  <input directory> <version string>', $
        '   GRP_TERRAIN    <input directory> <version string>', $
        '   GRP_RCCM       <input directory> <version string>', $
        '   AGP            <input directory> <version string>', $
        '   GP_GMP         <input directory> <version string>', $
        '   TC_STEREO      <input directory> <version string>', $
        '   TC_CLOUD       <input directory> <version string>', $
        '   TC_CLASSIFIERS <input directory> <version string>', $
        '   TC_ALBEDO      <input directory> <version string>', $
        '   AS_AEROSOL     <input directory> <version string>', $
        '   AS_LAND        <input directory> <version string>', $
        '   bxx-yy         (if available, copy products with this',  $
        '                   subsetted block range; b1-180 for all)', $
        '   <comma-separated list of orbit numbers, no spaces>', $
        '   <output directory name>','', $
        'Any of the first 10 lines specifying MISR product types', $
        'can be excluded, and these lines can be in any order.', $
        'However, the last 3 lines must be present.']

ell_use = 0
ter_use = 0
rcc_use = 0
agp_use = 0
gmp_use = 0
aer_use = 0
lan_use = 0
ste_use = 0
cls_use = 0
alb_use = 0

ell_databank = 0
ter_databank = 0
rcc_databank = 0
agp_databank = 0
gmp_databank = 0
aer_databank = 0
lan_databank = 0
ste_databank = 0
cls_databank = 0
alb_databank = 0

;---------------------------------------------------------------------
; Get the base directory and version numbers the user expects for
; each required MISR product.
;---------------------------------------------------------------------

filename = DIALOG_PICKFILE( $
                  TITLE='Select File Containing Copy Information', $
                  FILTER='*.txt', PATH=!SAV.WorkingDir, /MUST_EXIST)
IF (filename EQ '') THEN RETURN

OPENR, unit1, filename, /GET_LUN

buff = ''
lines = STRARR(13)

nlin = 0
WHILE ~EOF(unit1) DO BEGIN
   READF, unit1, buff
   IF (STRLEN(buff) EQ 0) THEN BREAK
   lines[nlin] = buff
   nlin += 1
   IF (nlin GE 13) THEN BREAK  ; don't read past max number of allowed lines
ENDWHILE

FOR ilin=0,nlin-4 DO BEGIN   ; loop over the initial MISR product lines

   toks = STRSPLIT(lines[ilin], ' ', /EXTRACT, COUNT=ntok)
   IF (ntok NE 3) THEN BEGIN
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
      RETURN
   ENDIF

   ;------------------------------------------------------------------
   ; Assign the correct product type for directory and version
   ; strings. Also test whether data to be copied are in the
   ; ../bank structure.
   ; NOTE - if files are referenced by their server address rather
   ;        than being automounted, then the server names and name
   ;        lengths may need to be changed below
   ;------------------------------------------------------------------

   FOR ityp=0,9 DO BEGIN
      IF (toks[0] EQ type_names[ityp]) THEN BEGIN
         CASE ityp OF
            0: BEGIN
                  ell_dir = toks[1]
                  ell_ver = toks[2]
                  ell_use = 1
                  toks1 = STRSPLIT(ell_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 5) THEN tokdum = STRMID(toks1[1],0,5)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'hrafn')) THEN $
                     ell_databank = 1
               END
            1: BEGIN
                  ter_dir = toks[1]
                  ter_ver = toks[2]
                  ter_use = 1
                  toks1 = STRSPLIT(ter_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 5) THEN tokdum = STRMID(toks1[1],0,5)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'hrafn')) THEN $
                     ter_databank = 1
               END
            2: BEGIN
                  rcc_dir = toks[1]
                  rcc_ver = toks[2]
                  rcc_use = 1
                  toks1 = STRSPLIT(rcc_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     rcc_databank = 1
               END
            3: BEGIN
                  agp_dir = toks[1]
                  agp_ver = toks[2]
                  agp_use = 1
                  toks1 = STRSPLIT(agp_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     agp_databank = 1
               END
            4: BEGIN
                  gmp_dir = toks[1]
                  gmp_ver = toks[2]
                  gmp_use = 1
                  toks1 = STRSPLIT(gmp_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 5) THEN tokdum = STRMID(toks1[1],0,5)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'hrafn')) THEN $
                     gmp_databank = 1
               END
            5: BEGIN
                  ste_dir = toks[1]
                  ste_ver = toks[2]
                  ste_use = 1
                  toks1 = STRSPLIT(ste_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     ste_databank = 1
               END
            6: BEGIN
                  cls_dir = toks[1]
                  cls_ver = toks[2]
                  cls_use = 1
                  toks1 = STRSPLIT(cls_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     cls_databank = 1
               END
            7: BEGIN
                  alb_dir = toks[1]
                  alb_ver = toks[2]
                  alb_use = 1
                  toks1 = STRSPLIT(alb_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     alb_databank = 1
               END
            8: BEGIN
                  aer_dir = toks[1]
                  aer_ver = toks[2]
                  aer_use = 1
                  toks1 = STRSPLIT(aer_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 8) THEN tokdum = STRMID(toks1[1],0,8)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'misrr150')) THEN $
                     aer_databank = 1
               END
            9: BEGIN
                  lan_dir = toks[1]
                  lan_ver = toks[2]
                  lan_use = 1
                  toks1 = STRSPLIT(lan_dir, !KON.Misc.Slash, /EXTRACT)
                  tokdum = ''
                  IF (STRLEN(toks1[1]) GE 5) THEN tokdum = STRMID(toks1[1],0,5)
                  IF ((toks1[0] EQ 'data' AND toks1[1] EQ 'bank') OR $
                      (toks1[0] EQ 'Volumes' AND tokdum EQ 'hrafn')) THEN $
                     lan_databank = 1
               END
         ENDCASE
      ENDIF
   ENDFOR
ENDFOR

;---------------------------------------------------------------------
; Read the block range - this range will be used to preferentially
; search for products subsetted to this range. If none are found
; for an orbit, the whole orbit will be copied if it is found.
;---------------------------------------------------------------------

toks = STRSPLIT(lines[nlin-3], ' ', /EXTRACT, COUNT=ntok)
subset_range = toks[0]
first_ch = STRMID(subset_range, 0, 1)
IF (first_ch NE 'b') THEN BEGIN
   msg1 = ['The first line after the product types must be', $
           'a block range starting with the character "b".', $
           'Correct this and try again.']
   rtrn = DIALOG_MESSAGE(msg1, /CENTER, /ERROR)
   RETURN
ENDIF

blks = STRSPLIT(subset_range, 'b-', /EXTRACT, COUNT=ntok)
IF (ntok NE 2) THEN BEGIN
   msg = ['The form of the block range must be bmm-nn', $
          'where "mm" and "nn" are block numbers.', $
          'Correct this and try again.']
   rtrn = DIALOG_MESSAGE(msg1, /CENTER, /ERROR)
   RETURN
ENDIF

IF (blks[0] EQ '1' AND blks[1] EQ '180') THEN BEGIN
   subset_range = ''
ENDIF ELSE BEGIN
   subset_range = '.' + subset_range
ENDELSE

;---------------------------------------------------------------------
; Read a series of orbit numbers in a comma-delimited list.
;---------------------------------------------------------------------

orbit_list = STRSPLIT(lines[nlin-2], ',', /EXTRACT, COUNT=ntok)
num_orbit = N_ELEMENTS(orbit_list)

;---------------------------------------------------------------------
; Read the destination directory of all the copied files.
;---------------------------------------------------------------------

;toks = STRSPLIT(lines[nlin-1], ' ', /EXTRACT, COUNT=ntok)
;dest_dir = toks[0]
dest_dir = lines[nlin-1]

IF ~FILE_TEST(dest_dir, /DIRECTORY) THEN BEGIN
   rtrn_val = MakeDirectory(dest_dir)
   rtrn_val = ChmodCatchError(dest_dir, '777'O)
ENDIF

FREE_LUN, unit1

;---------------------------------------------------------------------
; Create and open a log file in the same directory as the output.
;---------------------------------------------------------------------

log_file = dest_dir + !KON.Misc.Slash + 'CopyErrorLog.txt'

OPENW, unit, log_file, /GET_LUN

failpt = 0

CATCH, error_status
IF (error_status NE 0) THEN BEGIN
   mssg = 'Failed after ' + STRTRIM(STRING(failpt),2)
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   FREE_LUN, unit
   RETURN
ENDIF

;---------------------------------------------------------------------
; Loop over the orbits.
;---------------------------------------------------------------------

FOR iorbit=0,num_orbit-1 DO BEGIN

   OrbitNum = orbit_list[iorbit]

   ;------------------------------------------------------------------
   ; Construct a string of the correct length (5 characters) for
   ; the orbit number. Also compute the path from the orbit
   ; number and construct a path string of the correct length (3
   ; characters).
   ;------------------------------------------------------------------

   sorbit = STRTRIM(STRING(OrbitNum),2)
   IF (STRLEN(sorbit) EQ 3) THEN sorbit = '000' + sorbit
   IF (STRLEN(sorbit) EQ 4) THEN sorbit = '00'  + sorbit
   IF (STRLEN(sorbit) EQ 5) THEN sorbit = '0'   + sorbit

   failpt = 1

   spath = PathFromOrbit(LONG(OrbitNum))
   spath = STRTRIM(STRING(spath),2)
   IF (STRLEN(spath) EQ 1) THEN spath = '00' + spath
   IF (STRLEN(spath) EQ 2) THEN spath = '0'  + spath

   ;------------------------------------------------------------------
   ; Construct the pathnames of the MISR ellipsoid files. Copy the
   ; files for this orbit to the destination. Test for failure,
   ; print a message and keep going.
   ;------------------------------------------------------------------

   IF ell_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 2

      dest_ell_dir = dest_dir + !KON.Misc.Slash + type_names[0]
      IF ~FILE_TEST(dest_ell_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_ell_dir)
         rtrn_val = ChmodCatchError(dest_ell_dir, '777'O)
      ENDIF

      EllFiles = STRARR(9)
      EllFiles[*] = ell_begname + spath + '_O' + sorbit + '_' + $
                    cams[*] + '_' + ell_ver + '*hdf'
      EllFilesName = EllFiles

      IF (ell_databank EQ 1) THEN BEGIN
         EllFiles[*] = ell_dir + !KON.Misc.Slash + 'path' + spath + $
                       !KON.Misc.Slash + cams[*] + !KON.Misc.Slash + $
                       EllFiles
      ENDIF ELSE BEGIN
         EllFiles[*] = ell_dir + !KON.Misc.Slash + EllFiles[*]
      ENDELSE

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         BlockRangeTest, EllFiles[icam], subset_range, EllFile
         EllFiles[icam] = EllFile

         IF (~FILE_TEST(dest_ell_dir + $
             !KON.Misc.Slash + EllFilesName[icam])) THEN BEGIN
            IF (FILE_TEST(EllFiles[icam])) THEN BEGIN
               PRINTF, unit, 'Copying Ellipsoid file ' + EllFiles[icam]
               CopyFile, unit, EllFiles[icam], dest_ell_dir, status
               IF (status LT 0) THEN RETURN
            ENDIF ELSE BEGIN
               PRINTF, unit, 'Could not find Ellipsoid file ' + EllFilesName[icam]
            ENDELSE
         ENDIF
      ENDFOR

      EllFiles = 0
      EllFilesName = 0
   ENDIF

   ;------------------------------------------------------------------
   ; Construct the pathnames of the MISR terrain files. Copy the
   ; files for this orbit to the destination. Test for failure,
   ; print a message and keep going.
   ;------------------------------------------------------------------

   IF ter_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 3

      dest_ter_dir = dest_dir + !KON.Misc.Slash + type_names[1]
      IF ~FILE_TEST(dest_ter_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_ter_dir)
         rtrn_val = ChmodCatchError(dest_ter_dir, '777'O)
      ENDIF

      TerFiles = STRARR(9)
      TerFiles[*] = ter_begname + spath + '_O' + sorbit + '_' + $
                    cams[*] + '_' + ter_ver + '*hdf'
      TerFilesName = TerFiles
      
      IF (ter_databank EQ 1) THEN BEGIN
         TerFiles[*] = ter_dir + !KON.Misc.Slash + 'path' + spath + $
                       !KON.Misc.Slash + cams[*] + !KON.Misc.Slash + $
                       TerFiles
      ENDIF ELSE BEGIN
         TerFiles[*] = ter_dir + !KON.Misc.Slash + TerFiles[*]
      ENDELSE

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         BlockRangeTest, TerFiles[icam], subset_range, TerFile
         TerFiles[icam] = TerFile

         IF (~FILE_TEST(dest_ter_dir + $
             !KON.Misc.Slash + TerFilesName[icam])) THEN BEGIN
            IF (FILE_TEST(TerFiles[icam])) THEN BEGIN
               PRINTF, unit, 'Copying Terrain file ' + TerFiles[icam]
               CopyFile, unit, TerFiles[icam], dest_ter_dir, status
               IF (status LT 0) THEN RETURN
            ENDIF ELSE BEGIN
               PRINTF, unit, 'Could not find Terrain file ' + TerFilesName[icam]
            ENDELSE
         ENDIF
      ENDFOR

      TerFiles = 0
      TerFilesName = 0
   ENDIF

   ;------------------------------------------------------------------
   ; Construct the pathnames of the MISR rccm files. Copy the
   ; files for this orbit to the destination. Test for failure,
   ; print a message and keep going.
   ;------------------------------------------------------------------

   IF rcc_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 4

      dest_rcc_dir = dest_dir + !KON.Misc.Slash + type_names[2]
      IF ~FILE_TEST(dest_rcc_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_rcc_dir)
         rtrn_val = ChmodCatchError(dest_rcc_dir, '777'O)
      ENDIF

      RccFiles = STRARR(9)
      RccFiles[*] = rcc_begname + spath + '_O' + sorbit + '_' + $
                     cams[*] + '_' + rcc_ver + '*hdf'
      RccFilesName = RccFiles

      IF (rcc_databank EQ 1) THEN BEGIN
         RccFiles[*] = rcc_dir + !KON.Misc.Slash + 'path' + spath + $
                       !KON.Misc.Slash + cams[*] + !KON.Misc.Slash + $
                       RccFiles
      ENDIF ELSE BEGIN
         RccFiles[*] = rcc_dir + !KON.Misc.Slash + RccFiles[*]
      ENDELSE

      FOR icam=0,!KON.Instr.NCAM-1 DO BEGIN
         BlockRangeTest, RccFiles[icam], subset_range, RccFile
         RccFiles[icam] = RccFile

         IF (~FILE_TEST(dest_rcc_dir + $
             !KON.Misc.Slash + RccFilesName[icam])) THEN BEGIN
            IF (FILE_TEST(RccFiles[icam])) THEN BEGIN
               PRINTF, unit, 'Copying Rccm file ' + RccFiles[icam]
               CopyFile, unit, RccFiles[icam], dest_rcc_dir, status
               IF (status LT 0) THEN RETURN
            ENDIF ELSE BEGIN
               PRINTF, unit, 'Could not find Rccm file ' + RccFilesName[icam]
            ENDELSE
         ENDIF
      ENDFOR

      RccFiles = 0
      RccFilesName = 0
   ENDIF

   ;------------------------------------------------------------------
   ; Construct the pathnames of the other MISR files (AGP, GP_GMP,
   ; TC_CLASSIFIERS, AS_AEROSOL). Copy the files for this orbit
   ; to the destination. Test for failure, print a message and
   ; keep going.
   ;------------------------------------------------------------------

   IF agp_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 5

      dest_agp_dir = dest_dir + !KON.Misc.Slash + type_names[3]
      IF ~FILE_TEST(dest_agp_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_agp_dir)
         rtrn_val = ChmodCatchError(dest_agp_dir, '777'O)
      ENDIF

      AgpFile = agp_begname + spath + '_' + agp_ver + '*hdf'
      AgpFileName = AgpFile

      IF (agp_databank EQ 1) THEN BEGIN
         AgpFile = agp_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + AgpFile
      ENDIF ELSE BEGIN
         AgpFile = agp_dir + !KON.Misc.Slash + AgpFile
      ENDELSE

      BlockRangeTest, AgpFile, subset_range, AgpFile

      IF (~FILE_TEST(dest_agp_dir + !KON.Misc.Slash + AgpFileName)) THEN BEGIN
         IF (FILE_TEST(AgpFile)) THEN BEGIN
            PRINTF, unit, 'Copying AGP file ' + AgpFile
            CopyFile, unit, AgpFile, dest_agp_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find AGP file ' + AgpFileName
         ENDELSE
      ENDIF
   ENDIF

   IF gmp_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 6

      dest_gmp_dir = dest_dir + !KON.Misc.Slash + type_names[4]
      IF ~FILE_TEST(dest_gmp_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_gmp_dir)
         rtrn_val = ChmodCatchError(dest_gmp_dir, '777'O)
      ENDIF

      GmpFile = gmp_begname + spath + '_O' + sorbit + '_' + gmp_ver + '*hdf'
      GmpFileName = GmpFile

      IF (gmp_databank EQ 1) THEN BEGIN
         GmpFile = gmp_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + GmpFile
      ENDIF ELSE BEGIN
         GmpFile = gmp_dir + !KON.Misc.Slash + GmpFile
      ENDELSE

      BlockRangeTest, GmpFile, subset_range, GmpFile

      IF (~FILE_TEST(dest_gmp_dir + !KON.Misc.Slash + GmpFileName)) THEN BEGIN
         IF (FILE_TEST(GmpFile)) THEN BEGIN
            PRINTF, unit, 'Copying GMP file ' + GmpFile
            CopyFile, unit, GmpFile, dest_gmp_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find GMP file ' + GmpFileName
         ENDELSE
      ENDIF
   ENDIF

   IF ste_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 7

      dest_ste_dir = dest_dir + !KON.Misc.Slash + type_names[5]
      IF ~FILE_TEST(dest_ste_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_ste_dir)
         rtrn_val = ChmodCatchError(dest_ste_dir, '777'O)
      ENDIF

      SteFile = ste_begname + spath + '_O' + sorbit + '_' + ste_ver + '*hdf'
      SteFileName = SteFile

      IF (ste_databank EQ 1) THEN BEGIN
         SteFile = ste_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + SteFile
      ENDIF ELSE BEGIN
         SteFile = ste_dir + !KON.Misc.Slash + SteFile
      ENDELSE

      BlockRangeTest, SteFile, subset_range, SteFile

      IF (~FILE_TEST(dest_ste_dir + !KON.Misc.Slash + SteFileName)) THEN BEGIN
         IF (FILE_TEST(SteFile)) THEN BEGIN
            PRINTF, unit, 'Copying Stereo file ' + SteFile
            CopyFile, unit, SteFile, dest_ste_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find Stereo file ' + SteFileName
         ENDELSE
      ENDIF
   ENDIF

   IF cls_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 8

      dest_cls_dir = dest_dir + !KON.Misc.Slash + type_names[6]
      IF ~FILE_TEST(dest_cls_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_cls_dir)
         rtrn_val = ChmodCatchError(dest_cls_dir, '777'O)
      ENDIF

      ClsFile = cls_begname + spath + '_O' + sorbit + '_' + cls_ver +  '*hdf'
      ClsFileName = ClsFile

      IF (cls_databank EQ 1) THEN BEGIN
         ClsFile = cls_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + ClsFile
      ENDIF ELSE BEGIN
         ClsFile = cls_dir + !KON.Misc.Slash + ClsFile
      ENDELSE

      BlockRangeTest, ClsFile, subset_range, ClsFile

      IF (~FILE_TEST(dest_cls_dir + !KON.Misc.Slash + ClsFileName)) THEN BEGIN
         IF (FILE_TEST(ClsFile)) THEN BEGIN
            PRINTF, unit, 'Copying CLS file ' + ClsFile
            CopyFile, unit, ClsFile, dest_cls_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find Classifiers file ' + ClsFileName
         ENDELSE
      ENDIF
   ENDIF

   IF alb_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 9

      dest_alb_dir = dest_dir + !KON.Misc.Slash + type_names[7]
      IF ~FILE_TEST(dest_alb_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_alb_dir)
         rtrn_val = ChmodCatchError(dest_alb_dir, '777'O)
      ENDIF

      AlbFile = alb_begname + spath + '_O' + sorbit + '_' + alb_ver + '*hdf'
      AlbFileName = AlbFile

      IF (alb_databank EQ 1) THEN BEGIN
         AlbFile = alb_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + AlbFile
      ENDIF ELSE BEGIN
         AlbFile = alb_dir + !KON.Misc.Slash + AlbFile
      ENDELSE

      BlockRangeTest, AlbFile, subset_range, AlbFile

      IF (~FILE_TEST(dest_alb_dir + !KON.Misc.Slash + AlbFileName)) THEN BEGIN
         IF (FILE_TEST(AlbFile)) THEN BEGIN
            PRINTF, unit, 'Copying Albedo file ' + AlbFile
            CopyFile, unit, AlbFile, dest_alb_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find Albedo file ' + AlbFileName
         ENDELSE
      ENDIF
   ENDIF

   IF aer_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 10

      dest_aer_dir = dest_dir + !KON.Misc.Slash + type_names[8]
      IF ~FILE_TEST(dest_aer_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_aer_dir)
         rtrn_val = ChmodCatchError(dest_aer_dir, '777'O)
      ENDIF

      AerFile = aer_begname + spath + '_O' + sorbit + '_' + aer_ver + '*hdf'
      AerFileName = AerFile

      IF (aer_databank EQ 1) THEN BEGIN
         AerFile = aer_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + AerFile
      ENDIF ELSE BEGIN
         AerFile = aer_dir + !KON.Misc.Slash + AerFile
      ENDELSE

      BlockRangeTest, AerFile, subset_range, AerFile

      IF (~FILE_TEST(dest_aer_dir + !KON.Misc.Slash + AerFileName)) THEN BEGIN
         IF (FILE_TEST(AerFile)) THEN BEGIN
            PRINTF, unit, 'Copying Aerosol file ' + AerFile
            CopyFile, unit, AerFile, dest_aer_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find Aerosol file ' + AerFileName
         ENDELSE
      ENDIF
   ENDIF

   IF lan_use THEN BEGIN
      PRINTF, unit, ' '
      failpt = 11

      dest_lan_dir = dest_dir + !KON.Misc.Slash + type_names[9]
      IF ~FILE_TEST(dest_lan_dir, /DIRECTORY) THEN BEGIN
         rtrn_val = MakeDirectory(dest_lan_dir)
         rtrn_val = ChmodCatchError(dest_lan_dir, '777'O)
      ENDIF

      LanFile = lan_begname + spath + '_O' + sorbit + '_' + lan_ver + '*hdf'
      LanFileName = LanFile

      IF (lan_databank EQ 1) THEN BEGIN
         LanFile = lan_dir + !KON.Misc.Slash + 'path' + spath + $
                   !KON.Misc.Slash + LanFile
      ENDIF ELSE BEGIN
         LanFile = lan_dir + !KON.Misc.Slash + LanFile
      ENDELSE

      BlockRangeTest, LanFile, subset_range, LanFile

      IF (~FILE_TEST(dest_lan_dir + !KON.Misc.Slash + LanFileName)) THEN BEGIN
         IF (FILE_TEST(LanFile)) THEN BEGIN
            PRINTF, unit, 'Copying Land file ' + LanFile
            CopyFile, unit, LanFile, dest_lan_dir, status
            IF (status LT 0) THEN RETURN
         ENDIF ELSE BEGIN
            PRINTF, unit, 'Could not find Land file ' + LanFileName
         ENDELSE
      ENDIF
   ENDIF

ENDFOR

FREE_LUN, unit
CATCH, /CANCEL

END  ;  CopyFromServerToLocal
