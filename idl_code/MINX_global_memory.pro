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
PRO GetPlatformParams, IDL_version, MINX_version, MINX_platform, Fslash, $
                       Newline, SystemHomeDir, UserName, MINX_dir, $
                       xy_dims, MINX_graphics
;***************************************************************************
; Get parameters dependent on the platform and software version we're
; running with.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

;---------------------------------------------------------------------------
; Get IDL version and set MINX version.
;---------------------------------------------------------------------------

MINX_version = '4.0'

version = !VERSION.RELEASE
npos = STRPOS(version, '.')
IDL_version = FIX(STRMID(version,0,npos) + STRMID(version,npos+1,1))

;---------------------------------------------------------------------------
; Get the platform type. "unix" and Mac "OSX" use the same parameters.
;---------------------------------------------------------------------------

MINX_platform = 0
IF (!VERSION.OS_FAMILY EQ 'unix')    THEN MINX_platform = 1
IF (!VERSION.OS_FAMILY EQ 'Windows') THEN MINX_platform = 2

;---------------------------------------------------------------------------
; Set platform-dependent parameters.
; Set the slash/backslash in directory names for this platform.
; Also get the user's login ID and home directory.
;---------------------------------------------------------------------------

Fslash = PATH_SEP()

IF (MINX_platform EQ 2) THEN BEGIN  ; Windows
   Newline = STRING([10B, 13B])

   SystemHomeDir = GETENV('USERPROFILE')
   IF (SystemHomeDir EQ '') THEN SystemHomeDir = 'C:'
   UserName = ''
   npos = STRPOS(SystemHomeDir, Fslash, /REVERSE_SEARCH)
   IF (npos GT 0) THEN UserName = STRMID(SystemHomeDir, npos+1)
   SystemHomeDir += Fslash
ENDIF ELSE BEGIN              ; Unix/OSX
   Newline = STRING(10B) 

   UserName = GETENV('USER')
   SystemHomeDir = GETENV('HOME') + Fslash
ENDELSE

;---------------------------------------------------------------------------
; Get the directory MINX is stored in.
;---------------------------------------------------------------------------

CD, CURRENT=MINX_dir
MINX_dir += Fslash

;---------------------------------------------------------------------------
; Get the screen size.
;---------------------------------------------------------------------------

xy_dims = GET_SCREEN_SIZE()

;---------------------------------------------------------------------------
; Get the graphics card type for GPU acceleration - for future use.
;---------------------------------------------------------------------------

MINX_graphics = 0

END  ;  GetPlatformParams

;***************************************************************************
FUNCTION InitMINXParams
;***************************************************************************
; Create and initialize global parameters in system structures.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2

COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance
COMMON agp_data, Land_Water_Mask, Terrain_Hts
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
COMMON alb_data, Local_Albedo, Expans_Albedo, Restrict_Albedo
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

COMMON coord_data, CoordStruct
COMMON data_structs, region_data, linept_data
COMMON image_work, WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON AerosolBox, Wndw_Save

;---------------------------------------------------------------------------
; Define the name of the MINX preferences file that must reside in user's
; home directory as defined by his OS.
;---------------------------------------------------------------------------

MINX_PREF = 'MINX_preferences.sav'  ; default name of MINX preferences file
                                    ; containing global !VAR structure

;---------------------------------------------------------------------------
; Set flag for debug options:
;    0 = suppress debug options;  Set to 0 for production
;    1 = use debug options.
;    2 = use debug AND write to log file (not finished).
; If the DEBUG_FLAG is set to 1, MINX will remain in debug mode until it is
; changed here and MINX is rebuilt. The user can set the prod_or_dev flag to
; on in the remainder of any session by entering a -1 in the "First Block"
; field of the "Animate Cameras" dialog and clicking "OK".
;---------------------------------------------------------------------------

DEBUG_FLAG = 0

IF (DEBUG_FLAG EQ 0) THEN BEGIN
   prod_or_dev = 1  ; = see !KON.Misc.MINX_PRD_VER
   !EXCEPT = 0      ; never report exceptions
   !QUIET  = 1      ; suppress system messages
ENDIF ELSE BEGIN
   prod_or_dev = 2  ; = see !KON.Misc.MINX_DEV_VER
   !EXCEPT = 2      ; report exceptions after each IDL statement
   !QUIET  = 0      ; print system messages
ENDELSE
                                    
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
; Create global CONSTANTS structure !KON as a special system variable. Only
; create the structure if this is the first time here after loading MINX for
; a new session.
;
; NOTE - The MINX session .sav files depend on the content (the order and
; length of all structure elements) remaining the same. Don't change anything
; in !KON and !VAR structures unless you're willing to discard all previously
; saved MINX session .sav files.
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

DEFSYSV, '!KON', EXISTS=Cons_Exists  ; Test if !KON already exists in memory

IF (~ Cons_Exists) THEN BEGIN

   ;------------------------------------------------------------------------
   ; Get parameters dependent on the platform and software version we're
   ; running with.
   ;------------------------------------------------------------------------

   GetPlatformParams, IDL_version, MINX_version, MINX_platform, fslash, $
                      newline, sys_home_dir, user_name, MINX_dir, xy_dims, $
                      MINX_graphics
                      
   ;------------------------------------------------------------------------
   ; Determine whether the user has a preferences file. If so open it and
   ; restore the contents (into the !VAR global variable defined below).
   ; In that case, the definition of !VAR below will be skipped. Get the
   ; name of user's old data directory from the preferences file.
   ;------------------------------------------------------------------------

   pref_path_name = sys_home_dir + MINX_PREF

   ;------------------------------------------------------------------------
   ; Create miscellaneous constants structure.
   ;------------------------------------------------------------------------

   earth_radius = 6371000.0D  ; in m

   misc_vals = { $
      IDL_VERSION_NUM    : IDL_version,   $ ;
      MINX_VERSION_NUM   : MINX_version,  $ ; 
      MINX_PLATFORM      : MINX_platform, $ ; 
      MINX_GPU           : MINX_graphics, $ ; 
      MINX_DIRECTORY     : MINX_dir,      $ ; 
      MINX_PRD_VER       : 1,             $ ; 
      MINX_DEV_VER       : 2,             $ ; 
      MINX_PREF_PATH     : pref_path_name,$ ; 
      
      SystemHomeDir      : sys_home_dir,  $ ; fixed by OS - preferences file is here
      UserName           : user_name,     $ ; 
      Slash              : fslash,        $ ; 
      NewLine            : newline,       $ ; 
      ScreenX            : xy_dims[0],    $ ; screen size in X direction
      ScreenY            : xy_dims[1],    $ ; screen size in Y direction

      BADVALUE_REAL      : -9999.0,       $ ; 
      BADVALUE_FILL      : -9998,         $ ; 
      BADVALUE_ASCII     : -99.0,         $ ; 
      LARGE_POS_NUM      :  99999.0,      $ ; 
      LARGE_NEG_NUM      : -99999.0,      $ ; 

      DoubleRadeg        : (180.D / !DPI),$ ; 

      EarthRadius        : earth_radius,  $ ; approx earth radius in m
      EarthEquatorRadius : 6378000.0D,    $
      EarthPolarRadius   : 6357000.0D,    $
      EarthCircDiv360    : earth_radius * !DPI * 2.0 / 360.D, $ ; 

      INTERP_TYPE_SAMP   : 1, $             ; 3 methods for interpolating
      INTERP_TYPE_BILIN  : 2, $             ; pixel BRFs from 1100 m to 275 m
      INTERP_TYPE_CUBIC  : 3, $             ; resolution

      SymbID             : [1, 7, 2, 6, 5, 4], $ ; 
      SLOPE_MAX_THRESH1  : 40.0, $ ; ~88.6 degrees - if >, set to 40.0
      SLOPE_MAX_THRESH2  : 15.0, $ ; ~86.2 degrees - if >, set to 15.0
      LogUnit            : 88,   $ ; 

      PRINT_HISTOGRAMS   : 0 $ ;  To create and output histogram summary, set to 1.
   }

   ;------------------------------------------------------------------------
   ; Create instrument constants structure.
   ;------------------------------------------------------------------------

   instr_vals = { $
      NCAM  : 9,  $
      NBAND : 4,  $
      CAM_NAMES  : ['Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da'],        $
      CAM_ORDER  : [ 3,   5,   2,   6,   1,   7,   0,   8,   4],          $

      camera_names : ['OP','Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da',  $
                           'Df','Cf','Bf','Af','An','Aa','Ba','Ca','Da'], $
      DF  : 0, $
      CF  : 1, $
      BF  : 2, $
      AF  : 3, $
      AN  : 4, $
      AA  : 5, $ 
      BA  : 6, $
      CA  : 7, $
      DA  : 8, $

      BAND_NAMES : ['Blue', 'Green', 'Red', 'NIR'], $
      BLU  : 2, $
      GRN  : 1, $
      RED  : 0, $
      NIR  : 3, $
      RGBbands : [0,1,2], $

      ULC_SOM_ALONG    :  7460.750,   $   ; SOM along coord of up-left pix in blk 1 in km
      ULC_SOM_CROSS    :  527.45,     $   ; SOM cross coord of up-left pix in blk 1 in km
      SOM_PERIOD       :  98.88,      $   ; orbit period in minutes
      SOM_INCLIN       :  98.303820D, $   ; orbit inclination in deg
      SOM_LONG_PATH1   :  127.76054D, $   ; path 1 long. of ascend. orbit at equator in deg
      DEG_OFFSET       : -1.5450644D, $   ; adjacent path long. offsets in deg (=360/233)
      NUM_PATHS        :  233,        $
      NUM_BLOCKS       :  180,        $
      NO_RES_MASK      :  0,          $
      HI_RES_MASK      :  1,          $
      LO_RES_MASK      :  2,          $
      HI_RES_PIX_SIZE  :  0.275,      $
      LO_RES_PIX_SIZE  :  1.100,      $
      HI_RES_PIX_ALONG :  512,        $
      HI_RES_PIX_CROSS :  2048,       $
      LO_RES_PIX_ALONG :  128,        $
      LO_RES_PIX_CROSS :  512,        $
      HI_RES_BASE_NAME :  'LandBRF_275_meters',  $
      LO_RES_BASE_NAME :  'LandBRF_1100_meters', $

      MISR_altitude : 705.0, $
      CamViewAngles : [-70.5,-60.0,-45.6,-26.1,0.0,26.1,45.6,60.0,70.5], $
      MeanCamTimeFromAn : [-204.0,-145.0,-92.0,-45.5,0.0,45.5,92.0,145.0,204.0], $

      RelOffset : $  ;  offset of blocks relative to previous for all paths
         [  0.0,  0.0, 64.0,  0.0, 64.0,   0.0,   0.0,  0.0, 64.0,  0.0,  $
            0.0,  0.0,  0.0, 64.0,  0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  $
            0.0,  0.0,  0.0,  0.0,  0.0,   0.0,   0.0,-64.0,  0.0,  0.0,  $
            0.0,-64.0,  0.0,  0.0,-64.0,   0.0,   0.0,-64.0,  0.0,-64.0,  $
            0.0,-64.0,  0.0,-64.0,-64.0,   0.0, -64.0,  0.0,-64.0,-64.0,  $
            0.0,-64.0,-64.0,-64.0,  0.0, -64.0, -64.0,-64.0,-64.0,  0.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0, -64.0, -64.0,-64.0,-64.0,-64.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0, -64.0, -64.0,-64.0,-64.0,-64.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0,-128.0, -64.0,-64.0,-64.0,-64.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0, -64.0,-128.0,-64.0,-64.0,-64.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0, -64.0, -64.0,-64.0,-64.0,-64.0,  $
          -64.0,-64.0,-64.0,-64.0,-64.0, -64.0, -64.0,-64.0,-64.0,-64.0,  $
            0.0,-64.0,-64.0,-64.0,-64.0, -64.0,   0.0,-64.0,-64.0,-64.0,  $
            0.0,-64.0,-64.0,  0.0,-64.0,   0.0, -64.0,-64.0,  0.0,-64.0,  $
            0.0,-64.0,  0.0,  0.0,-64.0,   0.0, -64.0,  0.0,  0.0,-64.0,  $
            0.0,  0.0,  0.0,  0.0,-64.0,   0.0,   0.0,  0.0,  0.0,  0.0,  $
            0.0,  0.0,  0.0,  0.0,  0.0,   0.0,   0.0,  0.0,  0.0,  0.0,  $
            0.0,  0.0, 64.0,  0.0,  0.0,  64.0,   0.0,  0.0, 64.0,  0.0 ] $
   }

   ;------------------------------------------------------------------------
   ; Create colors structure. Load custom palette.
   ;------------------------------------------------------------------------

   ; Load custom palette and create 24-bit colors.
   
   AppColorTable = MINX_dir + 'data' + fslash + 'MINX_colors.tbl'
   AppTableNum = 49     ; must correspond to number in IDL color table
   LOADCT, AppTableNum, FILE=AppColorTable
   TVLCT, RR, GG, BB, /GET
   colors_24bit = RR + 256L * (GG + 256L * BB)

   default_palette = 4
   
   color_vals = {                      $
      gray1        : colors_24bit[0],  $
      blue         : colors_24bit[1],  $
      brown        : colors_24bit[2],  $
      red          : colors_24bit[3],  $
      green        : colors_24bit[4],  $
      blue1        : colors_24bit[5],  $
      yellow       : colors_24bit[6],  $
      purple       : colors_24bit[7],  $
      aqua         : colors_24bit[8],  $
      white        : colors_24bit[9],  $
      black        : colors_24bit[10], $
      lt_green     : colors_24bit[11], $
      lt_blue      : colors_24bit[12], $
      gray2        : colors_24bit[13], $
      gray3        : colors_24bit[14], $
      pink         : colors_24bit[15], $
      gray4        : colors_24bit[22], $
      blue2        : colors_24bit[47], $

      ColorVals    : ['0000FF'XUL, '00AA00'XUL, 'DD00DD'XUL, 'FF0000'XUL, $
                      '00FFFF'XUL, 'BBBB00'XUL, 'FFFFFF'XUL, '4444DD'XUL, $
                      '106080'XUL, '000000'XUL, 'FF6666'XUL, '66FF66'XUL, $
                      '444444'XUL, 'FF4444'XUL, 'FF0088'XUL], $

      ColorNames   : [      'red',     'green',   'magenta',      'blue', $
                         'yellow',      'aqua',     'white',      'pink', $
                          'brown',     'black',   'lt_blue',  'lt_green', $
                           'gray',     'blue2',    'purple'], $

      SymbolNames   : ['+', 'x', '*',  'square', 'triangle', 'diamond'], $

      BlueRedPalette : 33, $
      RainbowPalette : 13, $
      DefaultPalette :  default_palette  $
   }

   RR = 0 & GG = 0 & BB = 0
   LOADCT, default_palette

   ;------------------------------------------------------------------------
   ; Create IDL font type structure.
   ;------------------------------------------------------------------------

   font_typ_vals = {    $
      FONT_VECTOR : -1, $
      FONT_DEVICE :  0, $
      FONT_TRUTYP :  1  $
   }

   ;------------------------------------------------------------------------
   ; Create histogram constants structure.
   ;------------------------------------------------------------------------

   histo_vals = {           $
      HEIGHT_HIST   : 1,    $
      WIND_HIST     : 2,    $
      TAU_HIST      : 3,    $
      ANGEXP_HIST   : 4,    $
      SSA_HIST      : 5,    $
      TAUFRAC_HIST  : 6,    $
      HT_BIN_SIZE   : 0.25, $  ; width of height histograms bins (km)
      HT_BIN_CNTR   : 0,    $  ; bin edge is at 0.0 (_BIN_CNTR=1 not implement)
      WND_BIN_SIZE  : 1.0,  $  ; width of wind histogram bins (meters/sec)
      WND_BIN_CNTR  : 0,    $  ; bin edge is at 0.0
      TAU_BIN_SIZE  : 0.05, $  ; width of optical depth histogram bins
      TAU_BIN_CNTR  : 0,    $  ; bin edge is at 0.0
      ANG_BIN_SIZE  : 0.20, $  ; width of angstrom exponent histogram bins
      ANG_BIN_CNTR  : 0,    $  ; bin edge is at 0.0
      SSA_BIN_SIZE  : 0.01, $  ; width of aerosol SSA histogram bins
      SSA_BIN_CNTR  : 0,    $  ; bin edge is at 0.0
      TFR_BIN_SIZE  : 0.05, $  ; width of aerosol TFR histogram bins
      TFR_BIN_CNTR  : 0     $  ; bin edge is at 0.0
   }

   ;------------------------------------------------------------------------
   ; Create plot type constants structure.
   ;------------------------------------------------------------------------

   plot_typ_vals = { $
      DO_TIF  : 1,   $
      DO_JPG  : 2,   $
      DO_GIF  : 3,   $
      DO_PNG  : 4,   $
      DO_KML  : 5,   $
      DO_GTIF : 6,   $
      DO_MP4  : 7,   $
      DO_MGIF : 8    $
   }

   ;------------------------------------------------------------------------
   ; Create save image type constants structure.
   ;------------------------------------------------------------------------

   save_typ_vals = {          $
      SAVE_PLUME_ANIM    : 1, $
      SAVE_PLUME_POINTS  : 1, $
      SAVE_PLUME_CONTOUR : 1, $
      SAVE_HTPLOT_IMAGE  : 1, $
      SAVE_HTHIST_IMAGE  : 1, $
      SAVE_AERHIST_IMAGE : 1  $
   }

   ;------------------------------------------------------------------------
   ; Create a structure to hold the parameters that define the filename type
   ; to the DIALOG_PICKFILE and initialize the file names that will be read
   ; to null in case they are not present in the file.
   ; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
   ; CODE NEEDS TO BE REORGANIZED. 
   ;------------------------------------------------------------------------

   file_typ_vals = {          $
       NumFileTypes      :  0, $
       TypeVersion       :  0, $ ; ** First file **
       TypeL1B2          :  1, $
       TypeL1B2Ellipsoid :  2, $
       TypeL1B2Terrain   :  3, $
       TypeL1B2RCCM      :  4, $
       TypeBrowse        :  5, $
       TypeAgp           :  6, $
       TypeGpGmp         :  7, $
       TypeAerosol       :  8, $ ; older version 22
       TypeAerosolNew    :  9, $ ; new version in 2015?
       TypeLand          : 10, $
       TypeStereo        : 11, $
       TypeCloud         : 12, $
       TypeClass         : 13, $
       TypeAlbedo        : 14, $
       TypeCldMask       : 15, $
       TypeLandbrf       : 16, $
       TypeSaveSession   : 17, $
       TypeMISRsave      : 18, $ ; not done - for writing MISR data to text files
       TypeMarker        : 19, $
       TypeFire          : 20, $
       TypeModvolc       : 21, $
       TypeMOD14_URLfile : 22, $
       TypeMOD14file     : 23, $
       TypeMISRfile      : 24, $   
       TypePlumeRevu1    : 25, $
       TypePlumeRerevu   : 26, $
       TypeBiomeClass    : 27, $ ; ** Last file ** 
       TypeUtilProjDir   : 28, $ ; ** First directory - SEE TypeFirstDirUsed BELOW ** 
       TypeFireDir       : 29, $
       TypeMOD14dir      : 30, $
       TypeOverpassDir   : 31, $
       TypeJPGtoMP4Dir   : 32, $
       TypeRevuPlmDir    : 33, $
       TypeGeoRgnDir     : 34, $
       TypeGeoPlotDir    : 35, $
       TypeGeoPrntDir    : 36, $ 
       TypePrjVerifyDir  : 37, $ ; ** Last Directory - SEE TypeLastDirUsed BELOW ** 
       TypeFirstDirUsed  : 28, $ ; ALWAYS CHANGE THIS TO MATCH FIRST DIR ABOVE
       TypeLastDirUsed   : 37, $ ; ALWAYS CHANGE THIS TO MATCH LAST DIR ABOVE

       TE_Datasets       : ['Blue Radiance/RDQI',  'Green Radiance/RDQI',   $
                            'Red Radiance/RDQI',   'NIR Radiance/RDQI'],    $
       TE_Scale_Datasets : ['BlueConversionFactor','GreenConversionFactor', $
                            'RedConversionFactor', 'NIRConversionFactor'],  $
       AS_Dataset        : 'LandBRF' $
   }

   file_typ_vals.NumFileTypes = N_TAGS(file_typ_vals) - 6

   ;------------------------------------------------------------------------
   ; Create a structure that describes a select group of MISR products used
   ; for loading data from file efficiently.
   ; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
   ; CODE NEEDS TO BE REORGANIZED. 
   ;------------------------------------------------------------------------

   ; Array of names that references portions of grids in MISR data products.
   ; Each name is associated with a specific list of data fields in the grid
   ; of the respective products.

   product_names = ['BRF_CONV',  'AGP',                                 $
                    'GPGMP_SUN', 'GPGMP_AZI', 'GPGMP_ZEN', 'GPGMP_SCT', $
                    'GPGMP_GLT',                                        $
                    'AS_TAU_BE', 'AS_SSA',    'AS_AEX_BE', 'AS_FRC',    $
                    'AS_TAU_LR', 'AS_AEX_LR', 'AS_MIX',                 $
                    'AS_LAND1',  'AS_LAND2',                            $
                    'STER_11',   'STER_704',  'CLDZ_11',   'CLDC_11',   $
                    'CLD_176',   'TC_SVM',    'TC_ASCM',                $
                    'ALB_22',    'ALB_352a',  'ALB_352b',               $
                    'RCCM_MASK',                                        $
                    'ELL_GM',    'ELL_LM',    'TER_GM',    'TER_LM' ]

   ; Instance of data structure that holds parameters defining MISR products,
   ; grids and fields in HDF files. Allocate an array of these structures.
   ; Order of dimensions in returned data array is [across, along, block].

   MAX_NUM_GRIDS = N_ELEMENTS(product_names)
   MAX_FIELD_PER_GRID = 9
   MAX_DIMS_PER_FIELD = 3

   product_inst = { product_struct, $
                    ProductName : '', $
                    GridName    : '', $
                    NumField    :  0, $
                    FieldNames  : STRARR(MAX_FIELD_PER_GRID), $
                    BadValues   : FLTARR(MAX_FIELD_PER_GRID), $
                    Dims        : INTARR(MAX_DIMS_PER_FIELD), $
                    Starts      : INTARR(MAX_DIMS_PER_FIELD) }
                 
   prod_typ_vals = {  $
         OPEN_F : 1,  $         ; indicates file is to be opened
         KEEP_F : 2,  $         ; indicates file status is not to be changed 
         SHUT_F : 3,  $         ; indicates file is to be closed

         PROD_BRF_CONV  : 0, $  ; All these values get allocated during
         PROD_AGP       : 0, $  ; program initialization - (see bottom of
         PROD_GPGMP_SUN : 0, $  ; this function).
         PROD_GPGMP_AZI : 0, $
         PROD_GPGMP_ZEN : 0, $
         PROD_GPGMP_SCT : 0, $
         PROD_GPGMP_GLT : 0, $
         PROD_AS_TAU_BE : 0, $
         PROD_AS_SSA    : 0, $
         PROD_AS_AEX_BE : 0, $
         PROD_AS_FRC    : 0, $
         PROD_AS_TAU_LR : 0, $
         PROD_AS_AEX_LR : 0, $
         PROD_AS_MIX    : 0, $
         PROD_AS_LAND1  : 0, $
         PROD_AS_LAND2  : 0, $
         PROD_STER_11   : 0, $
         PROD_STER_704  : 0, $
         PROD_CLDZ_11   : 0, $
         PROD_CLDC_11   : 0, $
         PROD_CLD_176   : 0, $
         PROD_TC_SVM    : 0, $
         PROD_TC_ASCM   : 0, $
         PROD_ALB_22    : 0, $
         PROD_ALB_352a  : 0, $
         PROD_ALB_352b  : 0, $
         PROD_RCCM_MASK : 0, $
         PROD_ELL_GM    : 0, $
         PROD_ELL_LM    : 0, $
         PROD_TER_GM    : 0, $
         PROD_TER_LM    : 0, $

         MaxGrids  : MAX_NUM_GRIDS, $ 
         ProdNames : product_names, $
         ProdParms : REPLICATE(product_inst, MAX_NUM_GRIDS) $
   }

   ;------------------------------------------------------------------------
   ; Create an object type structure for digitized aerosol data. Use type
   ; codes to define aerosol types. Put arrays into the structure that
   ; defines attributes. These must be ordered identically with the type
   ; codes, so the codes can be used to index into the arrays. Update the
   ; names if XTRA objects are added.
   ;------------------------------------------------------------------------

   names = ['None',               $
            'Dust',               $
            'Smoke',              $
            'Volcanic ash',       $
            'Cloud/snow',         $
            'Jet contrail',       $
            'Not specified',      $
            'Extra option 1',     $
            'Extra option 2',     $
            'Direction line',     $
            'MODIS fire pixel',   $
            'Rsrch retr patch']

   abbrvs = ['',  'D', 'S', 'V', 'W', 'J', 'O', '1', '2', 'L', 'M', 'R']

   poly_color = ['',      'red',  'aqua',    'purple', 'green', 'blue2', $
                 'brown', 'pink', 'lt_blue', 'yellow', 'red',   'lt_green']

   aer_typ_vals = { $
      AER_DELETE_OBJ    : -1, $   ; indicates object is to be deleted
                                  ;    (not used as index!)
      AER_NULL_OBJ      :  0, $   ; indicates no object is selected
      AER_DUST_OBJ      :  1, $
      AER_SMOKE_OBJ     :  2, $
      AER_VOLCASH_OBJ   :  3, $
      AER_WATER_OBJ     :  4, $
      AER_JETTRAIL_OBJ  :  5, $   ; for contrails
      AER_OTHER_OBJ     :  6, $   ; other types not covered above
      AER_XTRA1_OBJ     :  7, $   ; for future use
      AER_XTRA2_OBJ     :  8, $   ; for future use
      DIR_LINE_OBJ      :  9, $   ; selected programmatically, not by user
      FIREPIXEL_OBJ     : 10, $   ; for MODIS fire pixels
      PATCH_OBJ         : 11, $   ; for research retrieval

      Name       : names,     $
      Abbrv      : abbrvs,    $
      PolyColor  : poly_color $
   }

   names = 0
   abbrvs = 0
   poly_color = 0

   ;------------------------------------------------------------------------
   ; Create 3 structures for digitized object geometric data:
   ; geometric-object-type structure, geometric-object-type-name structure
   ; and geometric-object-type-abbreviations structure. The last 2 must be
   ; ordered identically to the first, so those values can be used to index
   ; into this array.
   ;------------------------------------------------------------------------

   names = ['None', 'Point', 'Line', 'Polygon', 'Poly-Polygon']

   abbrvs = ['', 'T', 'L', 'P', 'Y']

   line_color = ['', 'red', 'aqua', 'blue', 'green']

   showit = [0, 1, 1, 0, 0]

   geom_typ_vals = { $
      GEOM_NULL_OBJ     :  0, $
      GEOM_POINT_OBJ    :  1, $  ; for future use
      GEOM_LINE_OBJ     :  2, $
      GEOM_POLYGON_OBJ  :  3, $
      GEOM_POLYPOLY_OBJ :  4, $  ; for future use

      Name      : names,      $
      Abbrv     : abbrvs,     $
      LineColor : line_color, $
      Show      : showit      $
   }

   names = 0
   abbrvs = 0
   line_color = 0
   showit = 0

   ;------------------------------------------------------------------------
   ; Create 3 structures for digitized wind retrieval data:
   ; wind-type structure, wind-type-name structure and wind-type-abbreviations
   ; structure. The last 2 must be ordered identically to the first, so those
   ; values can be used to index into this array.
   ;------------------------------------------------------------------------

   names = ['None', $
            'Direction unknown', $
            'Direction provided', $
            'Winds derived', $
            'Multiple wind directions']  ; for future use

   abbrvs = ['', 'N', 'W', 'C', 'M']

   symbols = ['', 'x', '+', 'square', 'triangle']

   wind_typ_vals = { $
      WIND_NULL_OBJ       :  0, $
      WIND_NO_DIREC_OBJ   :  1, $
      WIND_USER_DIREC_OBJ :  2, $
      WIND_MULT_DIREC_OBJ :  3, $
      WIND_CALC_DIREC_OBJ :  4, $

      Name   : names,  $
      Abbrv  : abbrvs, $
      Symbol : symbols $
   }

   names = 0
   abbrvs = 0
   symbols = 0

   ;------------------------------------------------------------------------
   ; Create 3 structures for bands used in retrieval:
   ; band-object-type structure, band-object-type-name and
   ; band-object-type-abbreviations. The last 2 must be ordered identically
   ; to the 1st so those values can be used to index into the array. 'C' is
   ; for composite band: (land=blue, water=red). 'Z' is for both bands, i.e.
   ; both red and blue bands are used in separate retrievals.
   ;------------------------------------------------------------------------

   names = ['Red', 'Blue', 'Both', 'R+B']

   abbrvs = ['R', 'B', 'Z', 'C']

   band_typ_vals = {  $
      NULL_BAND : -1, $
      RED_BAND  :  0, $
      BLUE_BAND :  1, $
      BOTH_BAND :  2, $
      RB_BAND   :  3, $

      Name  : names, $
      Abbrv : abbrvs $
   }

   names = 0
   abbrvs = 0

   ;------------------------------------------------------------------------
   ; Create digitized linked-list-node-type structure.
   ;------------------------------------------------------------------------

   node_typ_vals = {   $
      NULL_NODE   : 0, $
      POINT_NODE  : 1, $   ; for future use
      LINEPT_NODE : 2, $   ; 
      POLYPT_NODE : 3, $   ; 
      REGION_NODE : 4  $   ; 
   }

   ;------------------------------------------------------------------------
   ; Create correlation parameter structure.
   ;------------------------------------------------------------------------

   corr_parm_vals = {  $
      CORR_METHOD : 1, $
      M2_METHOD   : 2, $
      M3_METHOD   : 3, $
      M4_METHOD   : 4, $

   ; The following arrays contain reference patch sizes for passes 1 and 2:
   ;    (dim1 = camera, dim2 = matcher size option)

      CORR_SIZE_PASS1 : [[[11, 11,  9,  9,  9,  9,  9, 11, 11],   $
                          [11, 11,  9,  9,  9,  9,  9, 11, 11],   $
                          [13, 13, 11, 11, 11, 11, 11, 13, 13],   $
                          [13, 13, 11, 11, 11, 11, 11, 13, 13]]], $
      
      CORR_SIZE_PASS2 : [[[11,  9,  7,  7,  7,  7,  7,  9, 11],   $
                          [13, 11, 11,  9,  9,  9, 11, 11, 13],   $
                          [19, 17, 15, 13, 13, 13, 15, 17, 19],   $
                          [29, 27, 25, 23, 23, 23, 25, 27, 29]]], $

      CORR_SIZE_PASS  : [[[ 9,  9,  7,  7,  7,  7,  7,  9,  9],   $
                          [11, 11,  9,  9,  9,  9,  9, 11, 11],   $
                          [15, 15, 13, 13, 13, 13, 13, 15, 15],   $
                          [19, 19, 17, 17, 17, 17, 17, 19, 19]]]  $
   }

   ;------------------------------------------------------------------------
   ; Create digitizing parameter default values structure. These are copied
   ; to !SAV.Digitize structure only if it doesn't yet exist. Values can be
   ; changed by user.
   ;------------------------------------------------------------------------

   dig_zero = [aer_typ_vals.AER_NULL_OBJ, $
               geom_typ_vals.GEOM_NULL_OBJ, $
               wind_typ_vals.WIND_NULL_OBJ, $
               band_typ_vals.NULL_BAND]

   digitize_vals = { $
      DIG_STATE_ZERO  : dig_zero, $ ; cleaner way to reset DIG_STATE to zeros.
      JPEG_OR_MP4     : 1,    $ ; save animations as JPG (=0) or MP4 (=1) file(s)
      FRAME_PER_SEC   : 6,    $ ; MP4 frame rate; doesn't affect size or quality
      BIT_RATE   : 12000000L, $ ; MP4 bit rate; affects size and quality
      DRAW_AEROSOL    : 1,    $ ; create aerosol plot
      DOCS_ON_MAPS    : 1,    $ ; write orbit, date, ... on output map images
      PUB_QUALITY     : 0,    $ ; simplify plots for publication quality
      COMPARE_PGEHTS  : 0,    $ ; load and display heights/winds from PGE
      MIN_HGHT        : 0.25, $ ; minimum value of height allowed in retrieval (km)
      MAX_HGHT        : 6.0,  $ ; maximum value of height allowed in retrieval (km)
      MAX_WIND        : 30.0, $ ; max value of windspeed allowed in retrieval (m/s)
      BI_DIR_WIND     : 0,    $ ; one-way wind
      MATCHER_SM_LG   : 2,    $ ; medium size
      RELAX_THRESH    : 0,    $ ; option to relax retrieval thresholds
      USE_CAM_PAIRS   : 3,    $ ; A, B and C cams
      FIRST_CAM_USE   : 0,    $ ; must be 0 (Af), 2 (Bf), 4 (Cf) or 6 (Df)
      LAST_CAM_USE    : 5,    $ ; must be 1 (Aa), 3 (Ba), 5 (Ca) or 7 (Da)
      SAMP_SPAC_NODIR : 2.2,  $ ; when wind direction unknown
      SAMP_SPAC_DIR   : 1.1   $ ; when wind direction known
   }

   ;------------------------------------------------------------------------
   ; Create region data params structure. These are the options and default
   ; constants for display of derived data as colored pixels over digitized
   ; data regions in the animation window.
   ;------------------------------------------------------------------------

   datargn_vals = { $
    ; options for what type of data color key represents
      TYPE_DISP_CROSS  :  0, $  ; disparity across[9] ( -50 -> +50 pixel)
      TYPE_DISP_ALONG  :  1, $  ; disparity along[9]  ( -90 -> +90 pixel)
      TYPE_ZEROWIND_HT :  2, $  ; zero-wind height    (  -5 -> +30 km)
      TYPE_WINDCORR_HT :  3, $  ; wind-corr height    (-0.5 -> +30 km)
      TYPE_SMOOTHED_HT :  4, $  ; smoothed height     (-0.5 -> +30 km)
      TYPE_WIND_CROSS  :  5, $  ; wind speed across   ( -99 -> +99 m/s)
      TYPE_WIND_ALONG  :  6, $  ; wind speed along    ( -99 -> +99 m/s)
      TYPE_WIND_TOTAL  :  7, $  ; total wind speed    (   0 -> +99 m/s)

    ; array of data types for populating combo-box list
      TYPE_LIST      : ['Disparity across', 'Disparity along', $
                        'Zero-wind height', 'Wind-corrected height', $
                        'Smoothed height',  'Windspeed across', $
                        'Windspeed along',  'Total windspeed'], $

    ; units text for each data type
      DATA_UNITS     : ['Pixels', 'Pixels', 'Height (km)', 'Height (km)', $
                        'Height (km)', 'Wind (m/s)', 'Wind (m/s)', 'Wind (m/s)'], $

    ; minimum values in color key listed in order for each TYPE
      VALUE_MIN      : [-50.0, -90.0, -5.0, -0.5, -0.5, -99.0, -99.0,  0.0], $

    ; maximum values in color key listed in order for each TYPE
      VALUE_MAX      : [ 50.0,  90.0, 30.0, 30.0, 30.0,  99.0,  99.0, 99.0], $

    ; options for color background
      COLOR_WHITE    : 16777215, $  ; color key background white
      COLOR_BLACK    :        0, $  ; color key background black
      COLOR_GRAY     : 11184810, $  ; color key background gray
      COLOR_TRANSP   : 16777214, $  ; color key background transparent (NE WHITE !)

    ; options for where to display color key
      SHOW_DO_NOT       : 0, $  ; do not show color key
      SHOW_IN_NEW_WNDW  : 1, $  ; show color key in separate window
      SHOW_ON_ANIM_WNDW : 2, $  ; show color key on animation window
      SHOW_USER_SAVED   : 3, $  ; show color key on hidden window - user saved file
      SHOW_SYS_SAVED    : 4, $  ; show color key on hidden window - sys saved file

    ; options for whether to automatically compute and reset min and max values
      MINMAX_NONE  : 0, $  ; NEW_WNDW is drawn and hasn't changed - don't draw
      MINMAX_USER  : 1, $  ; use min and max from VALUE_MIN and VALUE_MAX
      MINMAX_SYS   : 2  $  ; calculate min and max from data on the fly
   }

   ;------------------------------------------------------------------------
   ; Create swath data product code structure. These are the options and
   ; default constants for display of colored pixels of MISR data products
   ; over the entire swath in the animation window.
   ; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
   ; CODE NEEDS TO BE REORGANIZED. 
   ;------------------------------------------------------------------------

   dataprod_codes = { $
    ; options for what type of data product to display
      TYPE_NONE           :   0, $
      TYPE_TERRAIN_HT     :   1, $
      TYPE_LANDH2O_MASK   :   2, $
      TYPE_BIOME_MAP      :   3, $
    SUNGEOM               :   4, $
      TYPE_SUN_ZENITH     :   5, $
      TYPE_SUN_AZIMUTH    :   6, $
    CAMZEN                :   7, $
      TYPE_CAM_ZEN_DF     :   8, $
      TYPE_CAM_ZEN_CF     :   9, $
      TYPE_CAM_ZEN_BF     :  10, $
      TYPE_CAM_ZEN_AF     :  11, $
      TYPE_CAM_ZEN_AN     :  12, $
      TYPE_CAM_ZEN_AA     :  13, $
      TYPE_CAM_ZEN_BA     :  14, $
      TYPE_CAM_ZEN_CA     :  15, $
      TYPE_CAM_ZEN_DA     :  16, $
    CAMAZI                :  17, $ ; CAMAZI must follow CAMZEN
      TYPE_CAM_AZI_DF     :  18, $
      TYPE_CAM_AZI_CF     :  19, $
      TYPE_CAM_AZI_BF     :  20, $
      TYPE_CAM_AZI_AF     :  21, $
      TYPE_CAM_AZI_AN     :  22, $
      TYPE_CAM_AZI_AA     :  23, $
      TYPE_CAM_AZI_BA     :  24, $
      TYPE_CAM_AZI_CA     :  25, $
      TYPE_CAM_AZI_DA     :  26, $
    CAMSCT                :  27, $
      TYPE_CAM_SCT_DF     :  28, $
      TYPE_CAM_SCT_CF     :  29, $
      TYPE_CAM_SCT_BF     :  30, $
      TYPE_CAM_SCT_AF     :  31, $
      TYPE_CAM_SCT_AN     :  32, $
      TYPE_CAM_SCT_AA     :  33, $
      TYPE_CAM_SCT_BA     :  34, $
      TYPE_CAM_SCT_CA     :  35, $
      TYPE_CAM_SCT_DA     :  36, $
    CAMGLT                :  37, $
      TYPE_CAM_GLT_DF     :  38, $
      TYPE_CAM_GLT_CF     :  39, $
      TYPE_CAM_GLT_BF     :  40, $
      TYPE_CAM_GLT_AF     :  41, $
      TYPE_CAM_GLT_AN     :  42, $
      TYPE_CAM_GLT_AA     :  43, $
      TYPE_CAM_GLT_BA     :  44, $
      TYPE_CAM_GLT_CA     :  45, $
      TYPE_CAM_GLT_DA     :  46, $
    AOD                   :  47, $
      TYPE_AER_BE_TAU_B   :  48, $
      TYPE_AER_BE_TAU_G   :  49, $
      TYPE_AER_BE_TAU_R   :  50, $
      TYPE_AER_BE_TAU_N   :  51, $
      TYPE_AER_LR_TAU_B   :  52, $
      TYPE_AER_LR_TAU_G   :  53, $
      TYPE_AER_LR_TAU_R   :  54, $
      TYPE_AER_LR_TAU_N   :  55, $
    SSA                   :  56, $
      TYPE_AER_SSA_BLU    :  57, $
      TYPE_AER_SSA_GRN    :  58, $
      TYPE_AER_SSA_RED    :  59, $
      TYPE_AER_SSA_NIR    :  60, $
      TYPE_AER_BE_ANGXP   :  61, $
      TYPE_AER_LR_ANGXP   :  62, $
    SIZE                  :  63, $
      TYPE_AER_FRC_SML    :  64, $
      TYPE_AER_FRC_MED    :  65, $
      TYPE_AER_FRC_LRG    :  66, $
    SHAPE                 :  67, $
      TYPE_AER_FRC_SPH    :  68, $
      TYPE_AER_FRC_NOS    :  69, $
    LAND                  :  70, $
      TYPE_LND_BHR_BLU    :  71, $
      TYPE_LND_BHR_GRN    :  72, $
      TYPE_LND_BHR_RED    :  73, $
      TYPE_LND_BHR_NIR    :  74, $
      TYPE_LND_DHR_BLU    :  75, $
      TYPE_LND_DHR_GRN    :  76, $
      TYPE_LND_DHR_RED    :  77, $
      TYPE_LND_DHR_NIR    :  78, $
      TYPE_LND_NDVI       :  79, $
      TYPE_LND_RPV1       :  80, $
      TYPE_LND_RPV2       :  81, $
      TYPE_LND_RPV3       :  82, $
    STEREO                :  83, $
      TYPE_STER_ZEROHT    :  84, $
      TYPE_STER_CORRHT    :  85, $
      TYPE_STER_SDCM      :  86, $
      TYPE_STER_WNDCRS    :  87, $
      TYPE_STER_WNDALG    :  88, $
    CLOUD                 :  89, $
      TYPE_CLDZ_ZEROHT    :  90, $
      TYPE_CLDZ_WNDCRS    :  91, $
      TYPE_CLDZ_CLDMSK    :  92, $
      TYPE_CLDC_CORRHT    :  93, $
      TYPE_CLDC_WNDCRS    :  94, $
      TYPE_CLDC_SDCM      :  95, $
      TYPE_CLD_MOTNHT     :  96, $
      TYPE_CLD_WNDCRS     :  97, $
      TYPE_CLD_WNDALG     :  98, $
      TYPE_CLD_CLDMSK     :  99, $
    SVM                   : 100, $
      TYPE_CLASS_SMOKE    : 101, $
      TYPE_CLASS_DUST     : 102, $
      TYPE_CLASS_CLOUD    : 103, $
      TYPE_CLASS_CLEAR    : 104, $
    ALB_L                 : 105, $
      TYPE_ALB_LOC_BLU    : 106, $
      TYPE_ALB_LOC_GRN    : 107, $
      TYPE_ALB_LOC_RED    : 108, $
      TYPE_ALB_LOC_NIR    : 109, $
      TYPE_ALB_LOC_BRD    : 110, $
    ALB_R                 : 111, $
      TYPE_ALB_RES_BLU    : 112, $
      TYPE_ALB_RES_GRN    : 113, $
      TYPE_ALB_RES_RED    : 114, $
      TYPE_ALB_RES_NIR    : 115, $
      TYPE_ALB_RES_BRD    : 116, $
    ALB_E                 : 117, $
      TYPE_ALB_EXP_BLU    : 118, $
      TYPE_ALB_EXP_GRN    : 119, $
      TYPE_ALB_EXP_RED    : 120, $
      TYPE_ALB_EXP_NIR    : 121, $
      TYPE_ALB_EXP_BRD    : 122  $
   }

   ;------------------------------------------------------------------------
   ; Create utilities params structure - these are default constants.
   ;------------------------------------------------------------------------

   date_time = SYSTIME()
   toks = STRSPLIT(date_time, ' ', /EXTRACT)
   year_str = toks[4]

   util_vals = { $
      ProjectName     : '',   $   ; Name of plume project.
      FirstValidBlock : 1,    $   ; Default values for first and last blocks
      LastValidBlock  : 180,  $   ;    to use in each orbit.
      FirstValidPath  : 1,    $   ; Default values for first and last paths - 
      LastValidPath   : 233,  $   ;    filters out whole orbits.
      FirstValidSamp  : 192,  $   ; These eliminate about 3/4 of the no-data
      LastValidSamp   : 1855, $   ;    edges of the MISR An swath.
      BegDate         : year_str + '-01-01', $
                                  ; Beginning date for which to retrieve data.
      EndDate          : year_str + '-12-31', $
                                  ; Ending date for which to retrieve data.
      MaxBlksPerLoad   :  8,  $   ; Default maximum number of blocks that an
                                  ;    orbit will be broken into for loading.
      ConfThresh       : 40,  $   ; Confidence level in % required for fire
                                  ;    pixel to pass muster.
      MinPixPower      : 10,  $   ; Minimum power in megawatts needed for fire
                                  ;    pixel to pass muster.
      MinBlkPower      : 50,  $   ; Minimum power in megawatts needed for a
                                  ;    block to pass muster.
      MinGrpPower     : 200,  $   ; Minimum power in megawatts needed for a
                                  ;    block-group to pass muster.
      NumFirePixGroup  :  5,  $   ; Default number of fire pixels required in
                                  ;    a block-group or group is rejected.
      NumFirePixOrbit  :  5,  $   ; Default # of fire pixels required in an orbit
                                  ;    or the orbit is rejected (only ModVolc).
      FirePixGetMethod :  0,  $   ; If =0, just use ModVolc fire pixel data so 
                                  ;    no MOD14 granules required.
                                  ; If =1, use ModVolc data to generate a list of
                                  ;    MOD14 granules to pull from Reverb site.
                                  ; If =2, user downloads MODIS granules from
                                  ;    Reverb site independently.
      ModisCollection : '005' $   ; Default MODIS MOD14 collection (version) number.
   }
   
   ;------------------------------------------------------------------------
   ; Create an MISR_ORBIT_REF table that provides adjustments for drift in
   ; the TERRA orbit as a function of time. The list of Julian dates
   ; represents ASCENDING node times (equator crossing time on dark side)
   ; for every 1000th orbit. The time at a specific block is calculated by
   ; adding (block number + ASCNODE_TO_BLK1) * per-block transit time to the
   ; ascending node time. See funct Get_OrbitTime_OrbitNumber for more info.
   ; Entries without orbit/date/time are extrapolated and should be replaced
   ; when updates are available.
   ;------------------------------------------------------------------------

   ;                    Julian time        orbit     date        time
   ;                  --------------       -----  -----------  --------
   orbit_ref_table = [2451530.84792D, $  ;     0  extrapolated
                      2451599.51745D, $  ;  1000  25 Feb 2000  01:14:34
                      2451668.18783D, $  ;  2000   3 May 2000  17:19:55
                      2451736.85696D, $  ;  3000  11 Jul 2000  09:23:28
                      2451805.52616D, $  ;  4000  18 Sep 2000  01:27:07
                      2451874.19467D, $  ;  5000  25 Nov 2000  17:29:46
                      2451942.86333D, $  ;  6000   2 Feb 2001  09:32:38
                      2452011.53161D, $  ;  7000  12 Apr 2001  01:34:58
                      2452080.19981D, $  ;  8000  19 Jun 2001  17:37:10
                      2452148.86869D, $  ;  9000  27 Aug 2001  09:40:21
                      2452217.53632D, $  ; 10000   4 Nov 2001  01:41:45
                      2452286.20435D, $  ; 11000  11 Jan 2002  17:43:42
                      2452354.87290D, $  ; 12000  21 Mar 2002  09:46:25
                      2452423.54264D, $  ; 13000  29 May 2002  01:50:51
                      2452492.21218D, $  ; 14000   5 Aug 2002  17:54:59
                      2452560.88142D, $  ; 15000  13 Oct 2002  09:58:41
                      2452629.55123D, $  ; 16000  21 Dec 2002  02:03:13
                      2452698.22027D, $  ; 17000  27 Feb 2003  18:06:38
                      2452766.88943D, $  ; 18000   7 May 2003  10:10:13
                      2452835.55849D, $  ; 19000  15 Jul 2003  02:13:40
                      2452904.22810D, $  ; 20000  21 Sep 2003  18:17:54
                      2452972.89852D, $  ; 21000  29 Nov 2003  10:23:19
                      2453041.56793D, $  ; 22000   6 Feb 2004  02:27:16
                      2453110.23683D, $  ; 23000  14 Apr 2004  18:30:29
                      2453178.90627D, $  ; 24000  22 Jun 2004  10:34:28
                      2453247.57547D, $  ; 25000  30 Aug 2004  02:38:07
                      2453316.24466D, $  ; 26000   6 Nov 2004  18:41:45
                      2453384.91458D, $  ; 27000  14 Jan 2005  10:46:26
                      2453453.58422D, $  ; 28000  24 Mar 2005  02:50:43
                      2453522.25365D, $  ; 29000  31 May 2005  18:54:42
                      2453590.92318D, $  ; 30000   8 Aug 2005  10:58:49
                      2453659.59252D, $  ; 31000  16 Oct 2005  03:02:40
                      2453728.26194D, $  ; 32000  23 Dec 2005  19:06:38
                      2453796.93158D, $  ; 33000   2 Mar 2006  11:10:55
                      2453865.60132D, $  ; 34000  10 May 2006  03:15:21
                      2453934.27109D, $  ; 35000  17 Jul 2006  19:19:49
                      2454002.94042D, $  ; 36000  24 Sep 2006  11:23:39
                      2454071.60990D, $  ; 37000   2 Dec 2006  03:27:42
                      2454140.28003D, $  ; 38000   8 Feb 2007  19:32:41
                      2454208.94952D, $  ; 39000  18 Apr 2007  11:36:45
                      2454277.61895D, $  ; 40000  26 Jun 2007  03:40:44
                      2454346.28864D, $  ; 41000   2 Sep 2007  19:45:05
                      2454414.95767D, $  ; 42000  10 Nov 2007  11:48:29
                      2454483.62756D, $  ; 43000  18 Jan 2008  03:53:08
                      2454552.29679D, $  ; 44000  26 Mar 2008  19:56:49
                      2454620.96679D, $  ; 45000   3 Jun 2008  12:01:37
                      2454689.63629D, $  ; 46000  11 Aug 2008  04:05:42
                      2454758.30577D, $  ; 47000  18 Oct 2008  20:09:45
                      2454826.97564D, $  ; 48000  26 Dec 2008  12:14:22
                      2454895.64529D, $  ; 49000   5 Mar 2009  04:18:40
                      2454964.31481D, $  ; 50000  12 May 2009  20:22:46
                      2455032.98389D, $  ; 51000  20 Jul 2009  12:26:15
                      2455101.65328D, $  ; 52000  27 Sep 2009  04:30:10
                      2455170.32292D, $  ; 53000   4 Dec 2009  20:34:27
                      2455238.99233D, $  ; 54000  11 Feb 2010  12:38:24
                      2455307.66179D, $  ; 55000  21 Apr 2010  04:42:25
                      2455376.33139D, $  ; 56000  28 Jun 2010  20:46:39
                      2455445.00076D, $  ; 57000   5 Sep 2010  12:50:32
                      2455513.67020D, $  ; 58000  13 Nov 2010  04:54:32
                      2455582.33953D, $  ; 59000  20 Jan 2011  20:58:22
                      2455651.00923D, $  ; 60000  30 Mar 2011  13:02:44
                      2455719.67873D, $  ; 61000   7 Jun 2011  05:06:49
                      2455788.34839D, $  ; 62000  14 Aug 2011  21:11:07
                      2455857.01773D, $  ; 63000  22 Oct 2011  13:14:58
                      2455925.68751D, $  ; 64000  30 Dec 2011  05:19:27
                      2455994.35708D, $  ; 65000   7 Mar 2012  21:23:38
                      2456063.02668D, $  ; 66000  15 May 2012  13:27:52
                      2456131.69605D, $  ; 67000  23 Jul 2012  05:31:45
                      2456200.36543D, $  ; 68000  29 Sep 2012  21:35:40
                      2456269.03520D, $  ; 69000   7 Dec 2012  13:40:08
                      2456337.70461D, $  ; 70000  14 Feb 2013  05:44:05
                      2456406.37452D, $  ; 71000  23 Apr 2013  21:48:45
                      2456475.04394D, $  ; 72000   1 Jul 2013  13:52:43
                      2456543.71351D, $  ; 73000   8 Sep 2013  05:56:54
                      2456612.38295D, $  ; 74000  15 Nov 2013  22:00:53
                      2456681.05237D, $  ; 75000  23 Jan 2014  14:04:51
                      2456749.72210D, $  ; 76000   2 Apr 2014  06:09:16
                      2456818.39189D, $  ; 77000   9 Jun 2014  22:13:46
                      2456887.06129D, $  ; 78000  17 Aug 2014  14:17:42
                      2456955.73079D, $  ; 79000  25 Oct 2014  06:21:47
                      2457024.40029D, $  ; 80000   1 Jan 2015  22:25:52
                      2457093.06968D, $  ; 81000  11 Mar 2015  14:29:47
                      2457161.73915D, $  ; 82000  extrapolated
                      2457230.40861D, $  ; 83000  extrapolated
                      2457299.07808D, $  ; 84000  extrapolated
                      2457367.74754D, $  ; 85000  extrapolated
                      2457436.41701D, $  ; 86000  extrapolated
                      2457505.08647D, $  ; 87000  extrapolated
                      2457573.75594D, $  ; 88000  extrapolated
                      2457642.42540D, $  ; 89000  extrapolated
                      2457711.09487D, $  ; 90000  extrapolated
                      2457779.76434D, $  ; 91000  extrapolated
                      2457848.43380D, $  ; 92000  extrapolated
                      2457917.10327D, $  ; 93000  extrapolated
                      2457985.77273D, $  ; 94000  extrapolated
                      2458054.44220D, $  ; 95000  extrapolated
                      2458123.11166D, $  ; 96000  extrapolated
                      2458191.78113D, $  ; 97000  extrapolated
                      2458260.45059D, $  ; 98000  extrapolated
                      2458329.12006D, $  ; 99000  extrapolated
                      2458396.41623D]    ; 99999  extrapolated

   ;------------------------------------------------------------------------
   ; Create user constants structure as system variable: KON.
   ;------------------------------------------------------------------------

   cnst = { Misc           : misc_vals,      $
            Instr          : instr_vals,     $
            Colors         : color_vals,     $
            FontTyp        : font_typ_vals,  $
            Histo          : histo_vals,     $
            PlotTyp        : plot_typ_vals,  $
            SaveTyp        : save_typ_vals,  $
            FileTyp        : file_typ_vals,  $
            ProdTyp        : prod_typ_vals,  $
            AerObjTyp      : aer_typ_vals,   $
            GeomObjTyp     : geom_typ_vals,  $
            WindObjTyp     : wind_typ_vals,  $
            BandObjTyp     : band_typ_vals,  $
            NodeObjTyp     : node_typ_vals,  $
            CorrParam      : corr_parm_vals, $
            Digitize       : digitize_vals,  $
            DataProd       : dataprod_codes, $
            DataRgn        : datargn_vals,   $
            Util           : util_vals,      $
            MISR_ORBIT_REF : orbit_ref_table $
   }

   misc_vals = 0
   instr_vals = 0
   color_vals = 0
   font_typ_vals = 0
   histo_vals = 0
   plot_typ_vals = 0
   save_typ_vals = 0
   file_typ_vals = 0
   prod_typ_vals = 0
   aer_typ_vals = 0
   geom_typ_vals = 0
   wind_typ_vals = 0
   band_typ_vals = 0
   node_typ_vals = 0
   corr_flag_vals = 0
   digitize_vals = 0
   dataprod_vals = 0
   datargn_vals = 0
   util_vals = 0
   dig_zero = 0
   orbit_ref_table = 0

   DEFSYSV, '!KON', cnst

ENDIF

Num_DataProd = N_ELEMENTS(TAG_NAMES(!KON.DataProd))

;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
; Create global VARIABLES structure !VAR as a special system variable.
;
; NOTE - The MINX session .sav files depend on the content (the order and
; length of all structure elements) remaining the same. Don't change anything
; in !KON, !SAV and !VAR structures unless you're willing to discard all
; previously saved MINX session .sav files. (really can't be helped)
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Test if there is already a !SAV global structure in memory that stores
; variables that will be saved in a file for access bewtween MINX sessions.
; If so, then use it and skip the initialization below.
;---------------------------------------------------------------------------

DEFSYSV, '!SAV', EXISTS=Save_Exists  ; Test if !SAV already exists in memory

IF (~ Save_Exists) THEN BEGIN

   ;------------------------------------------------------------------------
   ; If the !SAV global structure doesn't exist in memory, determine whether
   ; the user has a preferences file, which contains a copy of !SAV. If so,
   ; open it and restore the !SAV contents rather than reinitializing its
   ; values below. Get the name of the user's previous working data directory
   ; from the preferences file.
   ;------------------------------------------------------------------------

   old_working_dir = !KON.Misc.SystemHomeDir + 'Working' + !KON.Misc.Slash
   data_search = FILE_SEARCH(!KON.Misc.MINX_PREF_PATH, COUNT=num_fullname)

   IF (num_fullname) THEN BEGIN
      RESTORE, !KON.Misc.MINX_PREF_PATH
      DEFSYSV, '!SAV', SAV_RESTORE_NAME
      SAV_RESTORE_NAME = 0
      IF (!SAV.DfltFiles[0].SavePath EQ !KON.Misc.MINX_VERSION_NUM) THEN BEGIN
         old_working_dir = !SAV.WorkingDir
         Save_Exists = 1
      ENDIF
   ENDIF
   
ENDIF

;---------------------------------------------------------------------------
; If there is neither a !SAV parameter in memory nor in a preferences file,
; then ask the user for the directory to be used as working data directory.
; This is where a PlumeProjOrbitList would be stored for a project and where
; MINX output would be written. In this case, we must initialize parameters
; in the !SAV structure to the MINX defaults.
;---------------------------------------------------------------------------

IF (~ Save_Exists) THEN BEGIN
redo_home:
   new_working_dir = DIALOG_PICKFILE( $
               TITLE='Select or create a MINX working directory', $
               PATH=old_working_dir, FILE=old_working_dir, $
               GET_PATH=new_path, /DIRECTORY)
   IF (new_working_dir EQ '') THEN RETURN, -1

   IF (~ FILE_TEST(new_working_dir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(new_working_dir)
      IF (rtrn_val NE 0) THEN BEGIN
         mssg = ['Directory ' + new_working_dir, 'could not be created.', $
                 'Do you want to try again?']
         rtrn_val = DIALOG_MESSAGE(mssg, /QUESTION, /CENTER)
         IF (STRUPCASE(rtrn_val) EQ 'YES') THEN GOTO, redo_home
         RETURN, -1
      ENDIF
      rtrn_val = ChmodCatchError(new_working_dir, '777'O)
   ENDIF

   ;------------------------------------------------------------------------
   ; Create a filename structure to hold the last used file path for each
   ; type of file used. this is used by the DIALOG_PICKFILE function.
   ; Initialize the file names that will be read to null in case they are
   ; not present in the file.
   ; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
   ; CODE NEEDS TO BE REORGANIZED.
   ;------------------------------------------------------------------------
   
   MinxVer = STRTRIM(STRING(!KON.Misc.MINX_VERSION_NUM),2)
   
   default_files = REPLICATE({Parms, Typename : '', TypeTitle : '', SavePath : ''}, !KON.FileTyp.NumFileTypes)
   
   default_files[!KON.FileTyp.TypeVersion]      = {Parms, 'L1B2',          'MINX Version ID', MinxVer}
   default_files[!KON.FileTyp.TypeL1B2]         = {Parms, 'L1B2',          'Select Nadir Camera File', new_working_dir}
   default_files[!KON.FileTyp.TypeL1B2Ellipsoid]= {Parms, 'Ellipsoid',     'Select GRP_ELLIPSOID Camera File', new_working_dir}
   default_files[!KON.FileTyp.TypeL1B2Terrain]  = {Parms, 'Terrain',       'Select GRP_TERRAIN Camera File', new_working_dir}
   default_files[!KON.FileTyp.TypeL1B2RCCM]     = {Parms, 'RCCM',          'Select GRP_RCCM Camera File', new_working_dir}
   default_files[!KON.FileTyp.TypeBrowse]       = {Parms, 'Browse',        'Select GRP_ELLIPSOID_BR Camera File', new_working_dir}
   default_files[!KON.FileTyp.TypeAgp]          = {Parms, 'AGP',           'Select AGP File (Terrain Hts)', new_working_dir}
   default_files[!KON.FileTyp.TypeGpGmp]        = {Parms, 'GpGmp',         'Select GP_GMP File (Geometry)', new_working_dir}
   default_files[!KON.FileTyp.TypeAerosol]      = {Parms, 'AerosolOld',    'Select AS_AEROSOL File', new_working_dir}
   default_files[!KON.FileTyp.TypeAerosolNew]   = {Parms, 'AerosolNew',    'Select new AS_AEROSOL File', new_working_dir}
   default_files[!KON.FileTyp.TypeLand]         = {Parms, 'Land',          'Select AS_LAND File', new_working_dir}
   default_files[!KON.FileTyp.TypeStereo]       = {Parms, 'Stereo',        'Select TC_STEREO File', new_working_dir}
   default_files[!KON.FileTyp.TypeCloud]        = {Parms, 'Cloud',         'Select TC_CLOUD File', new_working_dir}
   default_files[!KON.FileTyp.TypeClass]        = {Parms, 'Class',         'Select TC_CLASSIFIERS File', new_working_dir}
   default_files[!KON.FileTyp.TypeAlbedo]       = {Parms, 'Albedo',        'Select TC_ALBEDO File', new_working_dir}
   default_files[!KON.FileTyp.TypeCldMask]      = {Parms, 'CldMask',       'Select Input Cloud Mask File', new_working_dir}
   default_files[!KON.FileTyp.TypeLandbrf]      = {Parms, 'Landbrf',       'Select LAND BRF File', new_working_dir}
   default_files[!KON.FileTyp.TypeSaveSession]  = {Parms, 'Session',       'Select Session .sav File', new_working_dir}
   default_files[!KON.FileTyp.TypeMISRsave]     = {Parms, 'MISRsave',      'Select Output Text File', new_working_dir}
   default_files[!KON.FileTyp.TypeMarker]       = {Parms, 'Marker',        'Select Marker Pixel File', new_working_dir}
   default_files[!KON.FileTyp.TypeFire]         = {Parms, 'Fire',          'Select Fire Pixel File', new_working_dir}
   default_files[!KON.FileTyp.TypeModvolc]      = {Parms, 'Modvolc',       'Select File Listing ModVolc Hot Spots', new_working_dir}
   default_files[!KON.FileTyp.TypeMOD14_URLfile]= {Parms, 'MOD14_URLfile', 'Select File Listing MOD14 Granule URLs', new_working_dir}
   default_files[!KON.FileTyp.TypeMOD14file]    = {Parms, 'MOD14file',     'Select File Listing MOD14 Granule Names', new_working_dir}
   default_files[!KON.FileTyp.TypeMISRfile]     = {Parms, 'MISRfile',      'Select File Listing Ordered MISR Products', new_working_dir}
   default_files[!KON.FileTyp.TypePlumeRevu1]   = {Parms, 'PlumeReview1',  'Select Text File from Pass1 Review', new_working_dir}
   default_files[!KON.FileTyp.TypePlumeRerevu]  = {Parms, 'PlumeRereview', 'Select Text File from Re-review Option', new_working_dir}
   default_files[!KON.FileTyp.TypeBiomeClass]   = {Parms, 'BiomeFile',     'Select IGBP Biome Grid File', !KON.Misc.MINX_DIRECTORY + 'data'}
   ; Files above and directories below
   default_files[!KON.FileTyp.TypeUtilProjDir]  = {Parms, 'UtilProjDir',   'Select/Create Dir for Plume Project Utilities', new_working_dir}
   default_files[!KON.FileTyp.TypeFireDir]      = {Parms, 'FireDir',       'Select/Create Dir for Output Fire Pixel Files', new_working_dir}
   default_files[!KON.FileTyp.TypeMOD14dir]     = {Parms, 'MOD14dir',      'Select/Create Dir for Downloaded MOD14 Granules', new_working_dir}
   default_files[!KON.FileTyp.TypeOverpassDir]  = {Parms, 'Overpass',      'Select/Create Dir for Overpass Lists', new_working_dir}
   default_files[!KON.FileTyp.TypeJPGtoMP4Dir]  = {Parms, 'JPGtoMP4',      'Select Project Dir w/ JPEGs to Convert to MP4', new_working_dir}
   default_files[!KON.FileTyp.TypeRevuPlmDir]   = {Parms, 'ReviewPlumes',  'Select Project Dir w/ Plumes to Review', new_working_dir}
   default_files[!KON.FileTyp.TypeGeoRgnDir]    = {Parms, 'ReorgProj',     'Select Project Dir w/ Plumes to Reorg into Geo Rgns', new_working_dir}
   default_files[!KON.FileTyp.TypeGeoPlotDir]   = {Parms, 'ReorgPlot',     'Select Dir where Images should be Saved', new_working_dir}
   default_files[!KON.FileTyp.TypeGeoPrntDir]   = {Parms, 'ReorgPrnt',     'Select New Project Dir where Plumes will be Moved', new_working_dir}
   default_files[!KON.FileTyp.TypePrjVerifyDir] = {Parms, 'VerifyProj',    'Select Project Dir with Orbits/Plumes to Verify', new_working_dir}
   
   ;------------------------------------------------------------------------
   ; Create height/wind digitizing params structure. These parameters store
   ; the results of user's entries in the digitizing dialog box - several of
   ; them are development version parameters.
   ;------------------------------------------------------------------------
   
   digitiz_params = { $
      DIG_STATE         : !KON.Digitize.DIG_STATE_ZERO, $
                              ; Code for current digitizing state. This is a copy
                              ;   of [AER_TYPE,GEOM_TYPE,WIND_TYPE,RETRIEVE_BAND_TYPE],
                              ;   but DIR_LINE_OBJ can be substituted for AER_TYPE.
      AER_TYPE          : !KON.AerObjTyp.AER_SMOKE_OBJ, $
                              ; Code for type of aerosol to retrieve:
                              ;   1 = dust;
                              ;   2 = smoke;
                              ;   3 = volcanic ash;
                              ;   4 = water/snow;
                              ;   5 = contrails;
                              ;   6 = other;
                              ;   7 = ground;
                              ;   8-10 = not used yet.
      JPEG_OR_MP4      : !KON.Digitize.JPEG_OR_MP4, $
                              ; Save animation in ht retrieval as JPG(=0) or MP4(=1)
      FRAME_PER_SEC    : !KON.Digitize.FRAME_PER_SEC, $
                              ; Setting for MP4 animations - doesn't affect size or quality.
      BIT_RATE         : !KON.Digitize.BIT_RATE, $
                              ; Setting for MP4 animations - affects size and quality.
      DRAW_AEROSOL     : !KON.Digitize.DRAW_AEROSOL, $ ; Create aerosol plot.
      DOCS_ON_MAPS     : !KON.Digitize.DOCS_ON_MAPS, $ ; Write orbit, date, ... on map images
      PUB_QUALITY      : !KON.Digitize.PUB_QUALITY, $
                              ; Simplify plots for publication quality.
      COMPARE_PGEHTS   : !KON.Digitize.COMPARE_PGEHTS, $
                              ; Load and display heights/winds from PGE
                              ;   0 = don't display
                              ;   1 = display from TC_STEREO
                              ;   2 = display from TC_CLOUD
      MIN_HGHT         : !KON.Digitize.MIN_HGHT, $
                              ; Minimum height above terrain in km for which wind-
                              ;   corrected heights will be retrieved. Default 0.25 km.
                              ;   For dust or near-ground aerosols, use ~0.050 km.
      MAX_HGHT         : !KON.Digitize.MAX_HGHT, $
                              ; Maximum height above sea level in km for which
                              ;    wind-corrected heights will be retrieved.
      MAX_WIND         : !KON.Digitize.MAX_WIND, $
                              ; Approximate maximum windspeed for retrievals.
      GEOM_TYPE        : !KON.GeomObjTyp.GEOM_POLYGON_OBJ, $
                              ; Code for type of geometric object to digitize:
                              ;   1 = point object;          ; for future use
                              ;   2 = line object;
                              ;   3 = polygon object (default);
                              ;   4 = poly-polygon object.   ; for future use
      WIND_TYPE        : !KON.WindObjTyp.WIND_USER_DIREC_OBJ, $
                              ; Code for how to handle wind direction:
                              ;   1 = no wind direction available;
                              ;   2 = wind direction specified by user (default);
                              ;   3 = spatially variable winds specified by user;  ; future use
                              ;   4 = no wind direction given - calculate winds.   ; not done
      BI_DIR_WIND      : !KON.Digitize.BI_DIR_WIND, $
                              ; Try retrieval w/ wind in 180 deg opposite dir also:
                              ;   0 = retrieve winds only in one direction (default);
                              ;   1 = retrieve winds in both directions.
      RETRIEVE_BAND_TYPE : !KON.BandObjTyp.BOTH_BAND, $
                              ; Code for which band to use for matching:
                              ;   0 = red high resolution;
                              ;   1 = blue low resolution;
                              ;   2 = both: red first, then blue (default);
                              ;   3 = red over water; blue over land.
      USE_BAND_NDX     : !KON.Instr.RED, $
                              ; Index into band of index arrays - red (0) or blue (2).
      MATCHER_SM_LG    : !KON.Digitize.MATCHER_SM_LG, $
                              ; Code for size of image matcher to use:
                              ;   1 = small matcher;
                              ;   2 = medium matcher (default);
                              ;   3 = large matcher;
                              ;   4 = extra-large matcher.
      RELAX_THRESH     : !KON.Digitize.RELAX_THRESH, $
                              ; If set, relax thresholds to allow more retrievals.
      USE_CAM_PAIRS    : !KON.Digitize.USE_CAM_PAIRS, $
                              ;   1-> A cams; 2-> A,B cams; 3-> A,B,C cams (default);
                              ;   4-> A,B,C,D cams; 5-> C,D cams
      FIRST_CAM_USE    : !KON.Digitize.FIRST_CAM_USE, $
                              ; Must be 0 (Af), 2 (Bf), 4 (Cf) or 6 (Df).
      LAST_CAM_USE     : !KON.Digitize.LAST_CAM_USE, $
                              ; Must be 1 (Aa), 3 (Ba), 5 (Ca) or 7 (Da).
      SAMP_SPAC_NODIR  : !KON.Digitize.SAMP_SPAC_NODIR, $
                              ; Spatial separation in km between points where
                              ;   retrievals are attempted for objects w/out wind
                              ;   direction specified.
      SAMP_SPAC_DIR    : !KON.Digitize.SAMP_SPAC_DIR, $
                              ; Spatial separation in km between points where
                              ;   retrievals are attempted for objects with wind
                              ;   direction specified.
                              ;--------------------
      TWO_RETRIEVALS   : 0, $ ; For retrieval testing - sub 2 tests for red/blue
                              ; 0,1 = red/blue
                              ; 2,3 = matcher variation
                              ; 4,5 = frwrd model algorithm
      PRINT_OFFSETS    : 0, $ ; Write disparity, heights, winds to orbit file.
                              ;   0 = don't write
                              ;   1 = write only disparity values
                              ;   2 = also write heights and winds
      SHOW_USER_DIALOG : 0, $ ; Show a dialog for user to enter feature
                              ;   description.
      AUTO_CORR_AN     : 0, $ ; Validate matcher positions by auto-correlating
                              ;   An camera.
      SHOW_REF_CMP     : 0, $ ; Draw and print reference and comparison camera
                              ;   image data.
      DRAW_CORR_MTRX   : 0, $ ; Show surface of image matcher correlation
                              ;   coefficients.
      DRAW_OFFSET_DIFF : 0, $ ; Show plots of "computed - observed" An/Cx
                              ;   pixel offsets.
      SHOW_3D_HEIGHTS  : 0, $ ; Determine whether to display 3D data cube.
                              ;   Only works if using idlde. The virtual
                              ;   machine does not allow it due of blocking.
                              ;   Comment out if not using.
                              ;   1 = HtPt symbols in volume;
                              ;   2 = Ht-WndC-WndA solution volume per camera;
                              ;   4 = Ht-WndC-WndA solution lines for all cams.
      PlumeOutDir     : '', $ ; Output directory for digitized plume images
                              ;   and text data.
      OverrideRgnName : ''  $ ; When non-null, use this name as region name
                              ;   being digitized rather than automatically
                              ;   assigned name.
   }

   ;------------------------------------------------------------------------
   ; Create height/wind retrieval params structure.
   ; NOTE - The first 6 parameters are critical in optimizing retrievals of
   ;        height and wind during the determination of the consensus value
   ;        of height and wind from multiple camera pairs at a point. They
   ;        establish linear relationships between "mean" retrieved height
   ;        or wind from camera pairs and the width of a zone of acceptance
   ;        for successful retrievals around a "central" height or wind: the
   ;        higher the aerosol, the wider the zone of acceptance. Therefore,
   ;        they act as filters to exclude noise and to include good results.
   ;        It's a careful balancing act requiring manual tuning. The height
   ;        threshold minima are particularly important in eliminating many
   ;        spurious retrievals near the terrain in low optical depth
   ;        aerosols over bright backgrounds, e.g. in Africa.
   ;------------------------------------------------------------------------
   
   htwind_params = { $
      ZR_HGT_THRESH_MIN   : 150.0, $; 'a' coeff in linear eq for zero-wind height (m)
      ZR_HGT_THRESH_SLOPE :  0.15, $; 'b' coeff in linear eq for zero-wind height (m)
      WC_HGT_THRESH_MIN   : 150.0, $; 'a' coeff in linear eq for wind-corr height (m)
      WC_HGT_THRESH_SLOPE :  0.15, $; 'b' coeff in linear eq for wind-corr height (m)
      WC_WND_THRESH_MIN   :  4.00, $; 'a' coeff in linear eq for wind speeds (m/s)
      WC_WND_THRESH_SLOPE :  0.30, $; 'b' coeff in linear eq for wind speeds (m/s)
      REGISTRATION_CORR   :     0, $; Was registration corrected? 0 -> No, 1 -> Yes.
      CORR_TARGET         :     0, $; Type of feature that's the target of correlation:
                                    ;    1 -> general misregistration correction;
                                    ;    2 -> misreg correction before digitizing plume;
                                    ;    3 -> also misreg correction before digitizing plume;
                                    ;    4 -> actual digitizing of plume/cloud.
      CORR_WNDW_SEP       :  40, $  ; Pixel separation between successive correlation
                                    ;   windows for misregistration correction.
      CORR_OP_WIDTH       :   0, $  ; Correlation operator width for misregistration only.
      SMOOTH_FLTR_SIZE_P1 :   7, $  ; Width of smoothing filters for passes 1 and 2
      SMOOTH_FLTR_SIZE_P2 :   1, $  ;    during misregistration correction.
      REF_WNDW_SIZE       :   0, $  ; Size of reference window for correlation.
                                    ;    This is set dynamically.
      INTERPOLATE_FCTR    :   5, $  ; The factor by which to increase resolution when
                                    ;    interpolating to find fractional offsets for
                                    ;    hi-res retrievals.
      OUTLIER_TUNING_FCTR : 1.0, $  ; Tuning factor used in rejecting correlation
                                    ;    points based on distance from the smoothed
                                    ;    offset surface.  Smaller values reject more
                                    ;    points.  Set to 20.0 for minimum rejection.
      SLOPE_TUNING_FCTR   : 0.0, $  ; Tuning factor used in rejecting correlation
                                    ;    points based on slope of correlation matrix.
                                    ;    Smaller values reject more points.
      MEANABSDEV_THRESH  : 0.002,$  ; Max absolute deviation values less than this
                                    ;    for a correlation surface cause rejection.
      FRACTION_VALID     : 0.50, $  ; The fraction of pixels that must be valid
                                    ;    in a correlation window to proceed with
                                    ;    the correlation.
      SHOW_OFFSET_SURF     :  1, $  ; Draw 2 surfaces representing the final
                                    ;    across and along offsets measured for image
                                    ;    over whole block.
      SHOW_SMOOTHING       :  1  $  ; Draw 6 surfaces representing across and along
                                    ;    offsets at correlation points: initial,
                                    ;    after smoothing and after outlier removal.
   }
   
   ;------------------------------------------------------------------------
   ; Load a default, IDL-predefined, palette. This can be modified by user.
   ;------------------------------------------------------------------------
   
   LOADCT, !KON.Colors.DefaultPalette
   TVLCT, RR, GG, BB, /GET
   color_palette = LONARR(!D.TABLE_SIZE)
   FOR icol=0,!D.TABLE_SIZE-1 DO $
      color_palette[icol] = RR[icol] + GG[icol] * 256L + BB[icol] * 65536L
   RR = 0 & GG = 0 & BB = 0
   
   ;------------------------------------------------------------------------
   ; Create utilities params structure. Parameters relating to the utility
   ; functions that post-process MINX files for inclusion in the online
   ; Plume Project database.
   ;------------------------------------------------------------------------
   
   util_params = { $
      ProjectName      : !KON.Util.ProjectName,   $
                             ; Name of plume project.
      FirstValidBlock  : !KON.Util.FirstValidBlock,  $
                             ; Value for first block to use in orbits.
      LastValidBlock   : !KON.Util.LastValidBlock,  $
                             ; Value for last block to use in orbits.
      FirstValidPath   : !KON.Util.FirstValidPath,  $
                             ; Values for first path - filters out orbits.
      LastValidPath    : !KON.Util.LastValidPath,  $
                             ; Values for last path - filters out orbits.
      FirstValidSamp   : !KON.Util.FirstValidSamp,  $
                             ; These eliminate part of the An MISR swath
                             ;    where there is never any valid data.
      LastValidSamp    : !KON.Util.LastValidSamp,  $
                             ; These eliminate part of the An MISR swath
                             ;    where there is never any valid data.
      BegDate          : !KON.Util.BegDate,  $
                             ; Beginning date for which to retrieve data.
      EndDate          : !KON.Util.EndDate,  $
                             ; Ending date for which to retrieve data.
      MaxBlksPerLoad   : !KON.Util.MaxBlksPerLoad,  $
                             ; Maximum number of blocks that an orbit
                             ;    will be broken into for loading.
      ConfThresh       : !KON.Util.ConfThresh,  $
                             ; Confidence level in % required for fire
                             ;    pixel to pass muster.
      MinPixPower      : !KON.Util.MinPixPower,  $
                             ; Minimum power in megawatts needed for fire
                             ;    pixel to pass muster.
      MinBlkPower      : !KON.Util.MinBlkPower,  $
                             ; Minimum power in megawatts needed for a
                             ;  block to pass muster.
      MinGrpPower      : !KON.Util.MinGrpPower,  $
                             ; Minimum power in megawatts needed for a
                             ;    block-group to pass muster.
      NumFirePixGroup  : !KON.Util.NumFirePixGroup,  $
                             ; Number of fire pixels required in orbit
                             ;    block-group or group is rejected.
      NumFirePixOrbit  : !KON.Util.NumFirePixOrbit,  $
                             ; Number of fire pixels required in an
                             ;    orbit or the orbit is rejected.
      FirePixGetMethod : !KON.Util.FirePixGetMethod,  $
                             ; If =0, just use ModVolc fire pixel data so
                             ;    no MOD14 granules required.
                             ; If =1, use ModVolc data to generate a list of
                             ;    MOD14 granules to pull from Reverb site.
                             ; If =2, user downloads MODIS granules from
                             ;    Reverb site independently.
      ModisCollection  : !KON.Util.ModisCollection,  $
                             ; MODIS collection (version) number.
      PlumeOption      : 0,   $ ;
      ProjlistIndex    : -1   $ ;
   }

   ;------------------------------------------------------------------------
   ; Create user variables structure as system variable: SAV.
   ;------------------------------------------------------------------------
   
   Save = { WorkingDir        : new_working_dir, $ ; user's working data directory
            PlumeProjDir      : new_working_dir, $
            MisrOrderWebSite  : 'http://l0dup05.larc.nasa.gov/MISR/cgi-bin/MISR/main.cgi', $
            ModisOrderFtpSite : 'http://e4ftl01.cr.usgs.gov/MODIS_Dailies_C/MOLT/MOD14.005', $
            ModVolcWebSite    : 'http://modis.higp.hawaii.edu/', $
            ColorPalette      : color_palette,   $
            DfltFiles         : default_files,   $
            Digitize          : digitiz_params,  $
            HtWind            : htwind_params,   $
            Util              : util_params      $
   }

   color_palette = 0
   default_files = 0
   digitize_parmas = 0
   htwind_params = 0
   util_params = 0
   
   DEFSYSV, '!Sav', Save
ENDIF

!SAV.HtWind.REGISTRATION_CORR = 0

WritePreferencesFile
ReadPreferencesFile  ; read again so system color tables can be updated

;---------------------------------------------------------------------------
; Test if there is already a !VAR global structure in memory that stores
; variables that will NOT be saved in a file for access between MINX
; sessions. If so, then use it and skip the initialization below.
;---------------------------------------------------------------------------

DEFSYSV, '!VAR', EXISTS=Vars_Exists  ; Test if !VAR already exists in memory

IF (~ Vars_Exists) THEN BEGIN
   
   ;------------------------------------------------------------------------
   ; Create BRF color scaling value structure for animation window.
   ;------------------------------------------------------------------------

   brf_scale_vals = { $
        ScalingMethod  : 2,   $  ; 0 = scale all 36 channels to same min/max BRFs
                                 ; 1 = scale each camera to same min/max BRFs
                                 ; 2 = scale each channels independently
        BandDisplayOpt : 1,   $  ; 1 = RGB, 2 = Blue, 3 = Green,
                                 ; 4 = Red, 5 = NIR
        Brightness     : 0.5, $  ; brightness scaling (0 -> 1); none = 0.5;
                                 ;   modifies upper end of color scale
        Contrast       : 0.5, $  ; gamma contrast scaling (0 -> 1); none = 0.5;
                                 ;   exponent ranges from 0.25 to 4.0
        NumHistBins    : 512, $  ; number of bins in the BRF histograms
        FracCutTailMin : 0.0, $  ; fraction of pixels in channel in low-end tail
                                 ;   that are cut off
        FracCutTailMax : 0.005 $ ; fraction of pixels in channel in high-end tail
                                 ;   that are cut off
   }

   ;------------------------------------------------------------------------
   ; Create current files-loaded structure.
   ;------------------------------------------------------------------------

   curr_files = { $
      BRF_Loaded       :  0, $
      AGP_Loaded       :  0, $
      GMP_Loaded       :  0, $
      AE1_Loaded       :  0, $  ; old aerosol AS_AEROSOL
      AE2_Loaded       :  0, $  ; new aerosol PGE9?
      LND_Loaded       :  0, $
      STR_Loaded       :  0, $  ; old stereo TC_STEREO
      CLD_Loaded       :  0, $  ; new stereo TC_CLOUD
      SVM_Loaded       :  0, $
      ALB_Loaded       :  0, $
      Fire_Loaded      :  0, $   ; = 1 if fire pixels are displayed
      Marker_Loaded    :  0, $   ; = 1 if marker points are displayed
      TerrHoles_Loaded :  0, $   ; = 1 if terrain holes are visible
      Biome_Loaded     :  0, $   ; = 1 if biome swath is available for display

      BRFfile          : '', $
      AGPfile          : '', $
      GMPfile          : '', $
      AE1file          : '', $  ; old aerosol AS_AEROSOL
      AE2file          : '', $  ; new aerosol PGE9?
      LNDfile          : '', $
      STRfile          : '', $  ; old stereo TC_STEREO
      CLDfile          : '', $  ; new stereo TC_CLOUD
      SVMfile          : '', $
      ALBfile          : '', $
      FireFile         : '', $
      MarkerFile       : '', $
      BiomeFile        : '', $  ; IGBP biome 0.05 deg grid from MCD12C1

      OverpassFile     : '', $
      CamFiles         : STRARR(18), $
      NadirFile        : '', $  ; used when CamFiles overwritten by .sav files
      PRINT_FILENAME   : ''  $
   }

   ;------------------------------------------------------------------------
   ; Create swath data product code structure. These are the options and
   ; default constants for display of colored pixels of MISR data products
   ; over the entire swath in the animation window. Clicking the Data Menu
   ; button causes MINX to show a menu based on these data.
   ;------------------------------------------------------------------------

   dprod = REPLICATE({ST, Field: '', Group: '', Units: '', BadVal: 0.0, MinAbs: 0.0, MaxAbs: 0.0, MinData: 0.0, MaxData: 0.0, MinDlg: 0.0, MaxDlg: 0.0}, $
                      Num_DataProd)
;                                                  Field Name              Group Name                Units        BadVal  AbsoluteMin/Max  Data Min/Max   Dialog Min/Max
;                                                -------------------      -------------            ------------   ------  --------------- --------------  --------------
   dprod[!KON.DataProd.TYPE_NONE]         = {ST, 'TYPE_NONE',             'NA',                    'NA',             0.0,   0.0,    0.0,     0.0,    0.0,    0.0,    0.0}
   dprod[!KON.DataProd.TYPE_TERRAIN_HT]   = {ST, 'Terrain height',        'Terrain height',        'Height (m)', -9999.0, -500.,  9000.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LANDH2O_MASK] = {ST, 'Land-Water mask',       'Land-Water mask',       'Unitless',      255.,    0.,     6.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_BIOME_MAP]    = {ST, 'Biome class map',       'Biome class map',       'Biome Code',    255.,    0.,    16.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_SUN_ZENITH]   = {ST, 'Sun zenith angle',      'Sun angles',            'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_SUN_AZIMUTH]  = {ST, 'Sun azimuth angle',     'Sun angles',            'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_DF]   = {ST, 'Zen: Df camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_CF]   = {ST, 'Zen: Cf camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_BF]   = {ST, 'Zen: Bf camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_AF]   = {ST, 'Zen: Af camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_AN]   = {ST, 'Zen: An camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_AA]   = {ST, 'Zen: Aa camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_BA]   = {ST, 'Zen: Ba camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_CA]   = {ST, 'Zen: Ca camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_ZEN_DA]   = {ST, 'Zen: Da camera',        'Camera zenith angle',   'Degrees',     -9999.,   0.0,   90.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_DF]   = {ST, 'Azim: Df camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_CF]   = {ST, 'Azim: Cf camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_BF]   = {ST, 'Azim: Bf camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_AF]   = {ST, 'Azim: Af camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_AN]   = {ST, 'Azim: An camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_AA]   = {ST, 'Azim: Aa camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_BA]   = {ST, 'Azim: Ba camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_CA]   = {ST, 'Azim: Ca camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_AZI_DA]   = {ST, 'Azim: Da camera',       'Camera azimuth angle',  'Degrees',     -9999., -40.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_DF]   = {ST, 'Scat: Df camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_CF]   = {ST, 'Scat: Cf camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_BF]   = {ST, 'Scat: Bf camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_AF]   = {ST, 'Scat: Af camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_AN]   = {ST, 'Scat: An camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_AA]   = {ST, 'Scat: Aa camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_BA]   = {ST, 'Scat: Ba camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_CA]   = {ST, 'Scat: Ca camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_SCT_DA]   = {ST, 'Scat: Da camera',       'Camera scatter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_DF]   = {ST, 'Glit: Df camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_CF]   = {ST, 'Glit: Cf camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_BF]   = {ST, 'Glit: Bf camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_AF]   = {ST, 'Glit: Af camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_AN]   = {ST, 'Glit: An camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_AA]   = {ST, 'Glit: Aa camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_BA]   = {ST, 'Glit: Ba camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_CA]   = {ST, 'Glit: Ca camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CAM_GLT_DA]   = {ST, 'Glit: Da camera',       'Camera glitter angle',  'Degrees',     -9999.,   0.0,  360.0,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_AER_BE_TAU_B] = {ST, 'Tau: Blue best est',    'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_BE_TAU_G] = {ST, 'Tau: Green best est',   'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_BE_TAU_R] = {ST, 'Tau: Red best est',     'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_BE_TAU_N] = {ST, 'Tau: NIR best est',     'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_LR_TAU_B] = {ST, 'Tau: Blue low resid',   'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_LR_TAU_G] = {ST, 'Tau: Green low resid',  'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_LR_TAU_R] = {ST, 'Tau: Red low resid',    'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_LR_TAU_N] = {ST, 'Tau: NIR low resid',    'Aerosol optical depth', 'Unitless',    -9999.,   0.0,    5.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_SSA_BLU]  = {ST, 'SSA: Blue band',        'Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_SSA_GRN]  = {ST, 'SSA: Green band',       'Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_SSA_RED]  = {ST, 'SSA: Red band',         'Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_SSA_NIR]  = {ST, 'SSA: NIR band',         'Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_BE_ANGXP] = {ST, 'Aer: ang exp best est', 'Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_LR_ANGXP] = {ST, 'Aer: ang exp low resid','Aerosol SS albedo',     'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_FRC_SML]  = {ST, 'Size: fraction small',  'Aerosol size fraction', 'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_FRC_MED]  = {ST, 'Size: fraction medium', 'Aerosol size fraction', 'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_FRC_LRG]  = {ST, 'Size: fraction large',  'Aerosol size fraction', 'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_FRC_SPH]  = {ST, 'Shape: fraction spher', 'Aerosol shape fraction','Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_AER_FRC_NOS]  = {ST, 'Shape: fract nonspher', 'Aerosol shape fraction','Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_LND_BHR_BLU]  = {ST, 'Land: BHR blue',        'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_BHR_GRN]  = {ST, 'Land: BHR green',       'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_BHR_RED]  = {ST, 'Land: BHR red',         'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_BHR_NIR]  = {ST, 'Land: BHR NIR',         'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_DHR_BLU]  = {ST, 'Land: DHR blue',        'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_DHR_GRN]  = {ST, 'Land: DHR green',       'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_DHR_RED]  = {ST, 'Land: DHR red',         'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_DHR_NIR]  = {ST, 'Land: DHR NIR',         'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_NDVI]     = {ST, 'Land: NDVI',            'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_RPV1]     = {ST, 'Land: RPV1 green',      'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_RPV2]     = {ST, 'Land: RPV2 green',      'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_LND_RPV3]     = {ST, 'Land: RPV3 green',      'Land surface',          'Unitless',    -9999.,   0.0,    1.0,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_STER_ZEROHT]  = {ST, 'STER: Zero-wind hts',   'TC Stereo',             'Height (km)', -9999.,  -3.0,   30.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_STER_CORRHT]  = {ST, 'STER: Wind-corr hts',   'TC Stereo',             'Height (km)', -9999.,  -0.5,   30.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_STER_SDCM]    = {ST, 'STER: Windspeed cross', 'TC Stereo',             'Wind (m/s)',  -9999., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_STER_WNDCRS]  = {ST, 'STER: Windspeed along', 'TC Stereo',             'Wind (m/s)',  -9999., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_STER_WNDALG]  = {ST, 'STER: CloudMask',       'TC Stereo',             'Unitless',        0.,    0.,    4.0,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_CLDZ_ZEROHT]  = {ST, 'CLD: Zero-wind hts',    'TC Cloud',              'Height (m)',  -9999.,-3000.,30000.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLDZ_WNDCRS]  = {ST, 'CLD: Zero crs-trk wind','TC Cloud',              'Wind (m/s)', -22222., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLDZ_CLDMSK]  = {ST, 'CLD: Zero cloud mask',  'TC Cloud',              'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLDC_CORRHT]  = {ST, 'CLD: Corr-wind hts',    'TC Cloud',              'Height (m)',  -9999., -500.,30000.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLDC_WNDCRS]  = {ST, 'CLD: Corr crs-trk wind','TC Cloud',              'Wind (m/s)', -22222., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLDC_SDCM]    = {ST, 'CLD: Corr cloud mask',  'TC Cloud',              'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLD_MOTNHT]   = {ST, 'CLD: Cloud Motion hts', 'TC Cloud',              'Height (m)',  -9999., -500.,30000.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLD_WNDCRS]   = {ST, 'CLD: Cross-track wind', 'TC Cloud',              'Wind (m/s)',  -9999., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLD_WNDALG]   = {ST, 'CLD: Along-track wind', 'TC Cloud',              'Wind (m/s)',  -9999., -99.0,   99.0,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLD_CLDMSK]   = {ST, 'CLD: Motion Cloud mask','TC Cloud',              'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_CLASS_SMOKE]  = {ST, 'SVM: smoke mask',       'SVM Scene classifier',  'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLASS_DUST]   = {ST, 'SVM: dust mask',        'SVM Scene classifier',  'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLASS_CLOUD]  = {ST, 'SVM: cloud mask',       'SVM Scene classifier',  'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_CLASS_CLEAR]  = {ST, 'SVM: land mask',        'SVM Scene classifier',  'Unitless',        0.,    0.,     4.,   9999., -9999.,  9999., -9999.}
;
   dprod[!KON.DataProd.TYPE_ALB_LOC_BLU]  = {ST, 'Local Alb: Blue band',  'TOA Local albedo',      'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_LOC_GRN]  = {ST, 'Local Alb: Green band', 'TOA Local albedo',      'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_LOC_RED]  = {ST, 'Local Alb: Red band',   'TOA Local albedo',      'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_LOC_NIR]  = {ST, 'Local Alb: NIR band',   'TOA Local albedo',      'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_LOC_BRD]  = {ST, 'Local Alb: Broadband',  'TOA Local albedo',      'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_RES_BLU]  = {ST, 'Restr Alb: Blue band',  'TOA Restrictive albedo','Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_RES_GRN]  = {ST, 'Restr Alb: Green band', 'TOA Restrictive albedo','Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_RES_RED]  = {ST, 'Restr Alb: Red band',   'TOA Restrictive albedo','Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_RES_NIR]  = {ST, 'Restr Alb: NIR band',   'TOA Restrictive albedo','Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_RES_BRD]  = {ST, 'Restr Alb: Broadband',  'TOA Restrictive albedo','Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_EXP_BLU]  = {ST, 'Expan Alb: Blue band',  'TOA Expansive albedo',  'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_EXP_GRN]  = {ST, 'Expan Alb: Green band', 'TOA Expansive albedo',  'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_EXP_RED]  = {ST, 'Expan Alb: Red band',   'TOA Expansive albedo',  'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_EXP_NIR]  = {ST, 'Expan Alb: NIR band',   'TOA Expansive albedo',  'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   dprod[!KON.DataProd.TYPE_ALB_EXP_BRD]  = {ST, 'Expan Alb: Broadband',  'TOA Expansive albedo',  'Unitless',    -9999.,   0.0,    1.5,   9999., -9999.,  9999., -9999.}
   
   ;------------------------------------------------------------------------
   ; Create linked list params structure. Parameters related to the storage
   ; of digitized data in linked lists.
   ;------------------------------------------------------------------------

   llist_params = { $
      pHeadObj      : PTR_NEW(), $
      pThisRgn      : PTR_NEW(), $
      pHeadPoint    : PTR_NEW(), $
      pThisPoint    : PTR_NEW(), $
      PrevEventX    : -1,        $
      PrevEventY    : -1         $
   }

   ;------------------------------------------------------------------------
   ; Create region data params structure. These are user-selected parameters
   ; for display of colored pixels representing retrieved data in digitized
   ; regions and in color bar describing data ranges.
   ;------------------------------------------------------------------------

   num_type = N_ELEMENTS(!KON.DataRgn.VALUE_MIN)

   datargn_params = { $
      COLORKEY_WNDW_ID : -1, $
      COLORKEY_DLG_ID  : -1, $
      SHOW             : !KON.DataRgn.SHOW_DO_NOT, $
      DATA_TYPE        : !KON.DataRgn.TYPE_WINDCORR_HT, $
              ; See !KON.DataRgn.<XXX> above for data types in each array below
              ; min value for all points in all plumes
      VALUE_MIN_ABS  : FLTARR(num_type) + 9999.0, $
              ; max value for all points in all plumes
      VALUE_MAX_ABS  : FLTARR(num_type) - 9999.0, $
              ; min value for all points in current plume for digitizing
      VALUE_MIN_DIG  : FLTARR(num_type) + 9999.0, $
              ; max value for all points in current plume for digitizing
      VALUE_MAX_DIG  : FLTARR(num_type) - 9999.0, $
              ; min value for all points in current plume for region dialog
      VALUE_MIN_DLG  : FLTARR(num_type) + 9999.0, $
              ; max value for all points in current plume for region dialog
      VALUE_MAX_DLG  : FLTARR(num_type) - 9999.0, $
      MINMAX_SOURCE  : !KON.DataRgn.MINMAX_USER, $
      BKGRND_COLOR   : !KON.DataRgn.COLOR_WHITE, $
      CAM_SEL        :   3, $
      SHOW_BAND_TYPE :   2, $  ; current retrieval type to display
                               ; 0 = show red, 1 = show blue, 
                               ; 2 = show both band retrievals
      POS_X          :  -1, $
      POS_Y          :  -1, $
      SIZE_X         : 100, $
      SIZE_Y         : 300  $
   }

   ;------------------------------------------------------------------------
   ; Create swath data params structure. These are parameters for display of
   ; colored pixels representing MISR data products over the entire swath in
   ; the animation window and in the color bar describing data ranges. These
   ; are user-selectable in a dialog box or side-effects of user actions.
   ;------------------------------------------------------------------------

   dataswath_params = { $
      COLORKEY_DISC_OBJ :  OBJ_NEW(), $ ; colorbar object w/ discrete values
      COLORKEY_DISC_ID  :  -1, $ ; window ID for colorbar w/ discrete values
      COLORKEY_CONT_ID  :  -1, $ ; window ID for colorbar w/ continuous values
      COLORKEY_DLG_ID   :  -1, $
      CURRENT_WNDW      :  -1, $
      SHOW              : !KON.DataRgn.SHOW_DO_NOT, $
      DATA_TYPE         : !KON.DataProd.TYPE_NONE, $
      OP_CHANGED        :  1, $ ; has the swath image changed in the OP window?
      BKGRND_COLOR      : !KON.DataRgn.COLOR_WHITE, $
      POS_X             :  -1, $
      POS_Y             :  -1, $
      SIZE_X            : 100, $
      SIZE_Y            : 300  $
   }
   
   ;------------------------------------------------------------------------
   ; Create automatic window params structure. Parameters related to the
   ; automatic capturing and saving to file of data windows during the
   ; digitizing process.
   ;------------------------------------------------------------------------

   autowndw_params = { $
      LEFT_EDGE      :   0, $
      RIGHT_EDGE     :   0, $
      TOP_EDGE       :   0, $
      BOTTOM_EDGE    :   0, $
      INITIATE       :   1, $
      CANCEL         :   0, $
      DFLT_PIX_SIZE  : 320, $
      MISRVis_rgborb :   1, $
      MISRVis_rgbcam : [1, 4, 7], $
      MISRVis_rgbbnd : [0, 1, 2]  $
   }

   ;------------------------------------------------------------------------
   ; Create BRF display params structure. Parameters related to the display
   ; of MISR BRF data as a function of camera angle, selected from the "BRF
   ; Plot params" button on the animation window. Stick 4 parameters for
   ; the Red/Blue 3D display here as well.
   ;------------------------------------------------------------------------

   brf_params = { $
      BRF_INIT_PLOT     : 2,  $  ; indicate left button clicked for new point
      PLOT_BAND_NUM     : 2,  $  ; band index number to plot in BRF plot window (2 = red)
      PLOT_SINGLE_PIXEL : 1,  $  ; 0 -> do not draw individual pixel BRFs
                                 ; 1 -> draw individual pixel BRFs
      PLOT_PIXEL_MEAN   : 0,  $  ; 0 -> do not draw pixel vicinity mean BRFs
                                 ; 1 -> draw pixel vicinity mean BRFs
      PIXEL_MEAN_SIZE   : 5,  $  ; # of pixels on edge of pixel vicinity square area
      USE_CONSTANT_MAX  : 0,  $  ; 0 -> scale each plot's BRFs to max for that pixel's plot
                                 ; 1 -> scale each plot's BRFs to max BRF for entire image
      SHOW_GEOM_BRF     : 0,  $  ; 0 -> do not show dialog box
                                 ; 1 -> show dialog box displaying geom and rad at point
      DO_BRF_PLOT       : 0,  $  ; 
      BRF_MAX_VAL       : [0.5, 0.5], $ ; [0] = deflt setting, [1] = user setting

      RedBlue3D_Cams    : 3,  $  ; default cam pair is [Af, An]
      RedBlueBright     : 100,$  ; default percent brightness
      ShowGroupInX      : 1,  $  ; group number across (1-4)
      ShowGroupInY      : 1   $  ; group number along (1-3)
   }

   ;------------------------------------------------------------------------
   ; Create window coords params structure. Parameters related to camera
   ; window animation.
   ;------------------------------------------------------------------------

   wndws_coords = { $
      LLPickCrds    : [[0.0D, 0.0D]], $
      SomPickCrds   : [[0.0, 0.0]],   $
      MisrPickCrds  : [[0, 0, 0]]     $
   }

   ;------------------------------------------------------------------------
   ; Create params for aerosol color research structure. Non-production.
   ;------------------------------------------------------------------------

   aerosol_rscrch = { $ ; aerosol classifier parameters for Dave D study;
      iToolID : -1,  $  ; available in development mode only.
      iColor  :  0,  $
      Xbeg    :  0,  $
      Ybeg    :  0,  $
      Xend    :  0,  $
      Yend    :  0,  $
      Show2D  :  0,  $
      AerosolClassify : 0 $
   }

   ;------------------------------------------------------------------------
   ; Create user variables structure as system variable: VAR.
   ;------------------------------------------------------------------------

   vars = { DEBUG_FLAG    : DEBUG_FLAG,      $
            ProdOrDev     : prod_or_dev,     $
            GlobalLocal   : 1,               $ ; 1 = global mode; 2 = local mode
            NumOrbits     : 1,               $ ; number of orbits to load (1 or 2)
            L1B2_TYPE     : 'ELLIPSOID',     $ ; type of L1B2 file loaded
            ClrScaleVals  : brf_scale_vals,  $
            CurrFiles     : curr_files,      $
            LinkList      : llist_params,    $
            DataRgn       : datargn_params,  $
            DataSwath     : dataswath_params,$
            DataProd      : dprod,           $
            AutoWndw      : autowndw_params, $
            BRFparms      : brf_params,      $
            WndwCoord     : wndws_coords,    $
            AerRsrch      : aerosol_rscrch,  $
            WORK_WNDW     : 0,               $
          ; the band order for the next 3 arrays is red=0, green=1, blue=2, nir=3
            RawImages     : PTRARR(!KON.Instr.NBAND, !KON.Instr.NCAM * 2), $
            NullPixelList : PTRARR(!KON.Instr.NBAND, !KON.Instr.NCAM * 2), $
            NullValueList : PTRARR(!KON.Instr.NBAND, !KON.Instr.NCAM * 2)  $
   }

   brf_scale_vals = 0
   curr_file = 0
   llist_params = 0
   dataswath_params = 0
   datargn_params = 0
   autowndw_params = 0
   brf_params = 0
   wndws_coords = 0
   aerosol_rscrch = 0
   draw_params = 0
   pt_pick_crds = 0
   timetest_params = 0

   DEFSYSV, '!VAR', vars
ENDIF

;---------------------------------------------------------------------------
; Update a few parameters.
;---------------------------------------------------------------------------

!SAV.Util.ModisCollection = STRMID(!SAV.MODISOrderFTPsite, 2, 3, /REVERSE_OFFSET)

;---------------------------------------------------------------------------
; Initialize a coordinate structure to contain parameters read from a file
; containing coordinates of points to be overlain on the screen image.
; These are called marker points in MINX.
;---------------------------------------------------------------------------
 
DEFSYSV, '!MRK', EXISTS=Mrks_Exists  ; Test if !MRK already exists in memory

IF (~ Mrks_Exists) THEN BEGIN
   title = ''
   max_objects = 3200
   max_obj_pts = 300
   
   PtPickCrds = { $
     ; object parameters
         title           : title, $
         max_objects     : max_objects, $
         num_objects     : 0, $
         object_shape    : STRARR(max_objects), $
         object_color    : STRARR(max_objects), $
         object_name     : STRARR(max_objects), $
     ; object point parameters
         max_obj_pts     : max_obj_pts, $
         num_obj_pts     : INTARR(max_objects), $
         obj_pt_lat      : DBLARR(max_objects,max_obj_pts), $
         obj_pt_lon      : DBLARR(max_objects,max_obj_pts), $
         obj_pt_block    : INTARR(max_objects,max_obj_pts), $
         obj_pt_275_line : INTARR(max_objects,max_obj_pts), $
         obj_pt_275_samp : INTARR(max_objects,max_obj_pts)  $
   }
   
   DEFSYSV, '!MRK', PtPickCrds
ENDIF

;---------------------------------------------------------------------------
; Initialize the linked list for containing digitized plume data.
;---------------------------------------------------------------------------

LListDefineStructs  ;  see top of file MINX_linked_list.pro

;---------------------------------------------------------------------------
; Initialize a coordinate structure to contain orbit parameters.
;---------------------------------------------------------------------------

IF (~ ISA(CoordStruct)) THEN BEGIN
   axa = GetCoordStructMember()
   CoordStruct = { orbit1:axa, orbit2:axa }
   axa = 0
ENDIF

;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
; Fill the product type data structure with parameters appropriate to each.
; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
; CODE NEEDS TO BE REORGANIZED. 
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------

Icnt = -1

;----------------------------------------------------------------------------
; BRFs from L1B2 - each camera is in a different file so one struct suffices.
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_BRF_CONV = (Icnt += 1)
Fields = ['BlueConversionFactor', 'GreenConversionFactor', $
          'RedConversionFactor',  'NIRConversionFactor', '','','','','']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_BRF_CONV] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_BRF_CONV], $
               'BRF Conversion Factors', numFields, Fields, $
               [-111., -111., -111., -111., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; AGP data - Fill value -12345. means there is no fill value available.
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_AGP = (Icnt += 1)
Fields = ['SurfaceFeatureID', 'AveSceneElev', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AGP], $
               'Standard', numFields, Fields, $
               [-1.0, -500.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [512, 128, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; GP_GMP data. Any value less than 0.0 is a bad value, so use -1.
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_GPGMP_SUN = (Icnt += 1)
Fields = ['SolarAzimuth', 'SolarZenith', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SUN] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_GPGMP_SUN], $
               'GeometricParameters', numFields, Fields, $
               [-111.,-111., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_GPGMP_AZI = (Icnt += 1)
Fields = ['DfAzimuth', 'CfAzimuth', 'BfAzimuth', 'AfAzimuth', 'AnAzimuth', $
          'AaAzimuth', 'BaAzimuth', 'CaAzimuth', 'DaAzimuth']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_AZI] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_GPGMP_AZI], $
               'GeometricParameters', numFields, Fields, $
               [-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_GPGMP_ZEN = (Icnt += 1)
Fields = ['DfZenith', 'CfZenith', 'BfZenith', 'AfZenith', 'AnZenith', $
          'AaZenith', 'BaZenith', 'CaZenith', 'DaZenith']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_ZEN] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_GPGMP_ZEN], $
               'GeometricParameters', numFields, Fields, $
               [-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_GPGMP_SCT = (Icnt += 1)
Fields = ['DfScatter', 'CfScatter', 'BfScatter', 'AfScatter', 'AnScatter', $
          'AaScatter', 'BaScatter', 'CaScatter', 'DaScatter']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SCT] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_GPGMP_SCT], $
               'GeometricParameters', numFields, Fields, $
               [-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_GPGMP_GLT = (Icnt += 1)
Fields = ['DfGlitter', 'CfGlitter', 'BfGlitter', 'AfGlitter', 'AnGlitter', $
          'AaGlitter', 'BaGlitter', 'CaGlitter', 'DaGlitter']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_GLT] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_GPGMP_GLT], $
               'GeometricParameters', numFields, Fields, $
               [-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.,-111.], $
               [32, 8, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; AS_AEROSOL - all fields used are floats and all have -9999.0 as bad value.
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_AS_TAU_BE = (Icnt += 1)
Fields = ['RegBestEstimateSpectralOptDepth[0]','RegBestEstimateSpectralOptDepth[1]', $
          'RegBestEstimateSpectralOptDepth[2]','RegBestEstimateSpectralOptDepth[3]', $
          '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_TAU_BE] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_TAU_BE], $
               'RegParamsAer', numFields, Fields, $
               [-9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_SSA = (Icnt += 1)
Fields = ['RegBestEstimateSpectralSSA[0]', 'RegBestEstimateSpectralSSA[1]', $
          'RegBestEstimateSpectralSSA[2]', 'RegBestEstimateSpectralSSA[3]', $
          '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_SSA] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_SSA], $
               'RegParamsAer', numFields, Fields, $
               [-9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_AEX_BE = (Icnt += 1)
Fields = ['RegBestEstimateAngstromExponent', '', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_AEX_BE] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_AEX_BE], $
               'RegParamsAer', numFields, Fields, $
               [-9999., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_FRC = (Icnt += 1)
Fields = ['RegBestEstimateSpectralOptDepthFraction[1][0]', $
          'RegBestEstimateSpectralOptDepthFraction[1][1]', $
          'RegBestEstimateSpectralOptDepthFraction[1][2]', $
          'RegBestEstimateSpectralOptDepthFraction[1][3]', $
          'RegBestEstimateSpectralOptDepthFraction[1][4]', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_FRC] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_FRC], $
               'RegParamsAer', numFields, Fields, $
               [-9999., -9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_TAU_LR = (Icnt += 1)
Fields = ['RegLowestResidSpectralOptDepth[0]', 'RegLowestResidSpectralOptDepth[1]', $
          'RegLowestResidSpectralOptDepth[2]', 'RegLowestResidSpectralOptDepth[3]', $
          '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_TAU_LR] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_TAU_LR], $
               'RegParamsPerMixture', numFields, Fields, $
               [-9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_AEX_LR = (Icnt += 1)
Fields = ['RegLowestResidAngstromExponent', '', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_AEX_LR] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_AEX_LR], $
               'RegParamsPerMixture', numFields, Fields, $
               [-9999., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_MIX = (Icnt += 1)
Fields = ['RegLowestResidMixture', '', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_MIX] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_MIX], $
               'RegParamsPerMixture', numFields, Fields, $
               [-9999., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; AS_LAND data
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_AS_LAND1 = (Icnt += 1)
Fields = ['LandBHR[0]', 'LandBHR[1]', 'LandBHR[2]', 'LandBHR[3]', $
          'LandDHR[0]', 'LandDHR[1]', 'LandDHR[2]', 'LandDHR[3]', ''] 
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND1] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_LAND1], $
               'SubregParamsLnd', numFields, Fields, $
               [253., 253., 253., 253., 253., 253., 253., 253., 0.0], $
               [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_AS_LAND2 = (Icnt += 1)
Fields = ['NDVI', 'BRFModParam1[1]', 'BRFModParam2[1]', 'BRFModParam3[1]', $
          '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND2] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_AS_LAND2], $
               'SubregParamsLnd', numFields, Fields, $
               [253., 65533., 253., 253., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [512, 128, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; Old PGE8a TC_STEREO data
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_STER_11 = (Icnt += 1)
Fields = ['PrelimERStereoHeight_WithoutWinds', 'StereoHeight_BestWinds', $
          'SDCM_WithoutWinds', 'SDCM_BestWinds', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_11] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_STER_11], $
               'SubregParams', numFields, Fields, $
               [-9999.,-9999., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [512, 128, 1], [0, 0, 0] }

; NOTE - regions need to be reduced in size by x 4 to accommodate block assembly
;        this is done when data are loaded
!KON.ProdTyp.PROD_STER_704 = (Icnt += 1)
Fields = ['EWCloudMotionSpeedLowCloudBin',  'NSCloudMotionSpeedLowCloudBin', $
          'EWCloudMotionSpeedHighCloudBin', 'NSCloudMotionSpeedHighCloudBin', $
          '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_704] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_STER_704], $
               'DomainParams', numFields, Fields, $
               [-9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0, 0.0], $
               [8, 2, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; New stereo Cloud PGE data
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_CLDZ_11 = (Icnt += 1)
Fields = ['CloudTopHeight_WithoutWindCorrection', $
          'CloudMotionCrossTrack_WithoutWindCorrection', $
          'StereoDerivedCloudMask_WithoutWindCorrection', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDZ_11] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_CLDZ_11], $
               'Stereo_WithoutWindCorrection_1.1_km', numFields, Fields, $
               [-9999., -22222., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_CLDC_11 = (Icnt += 1)
Fields = ['CloudTopHeight', 'CloudMotionCrossTrack', 'StereoDerivedCloudMask', $
          '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDC_11] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_CLDC_11], $
               'Stereo_1.1_km', numFields, Fields, $
               [-9999., -22222., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_CLD_176 = (Icnt += 1)
Fields = ['CloudTopHeightOfMotion', 'CloudMotionNorthward', $
          'CloudMotionEastward', 'MotionDerivedCloudMask', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLD_176] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_CLD_176], $
               'Motion_17.6_km', numFields, Fields, $
               [-9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
               [32, 8, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; SVM data. No data available is 0.
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_TC_SVM = (Icnt += 1)
Fields = ['SVMCloudConfidenceLevel','SVMSmokeConfidenceLevel', $
          'SVMDustConfidenceLevel', 'SVMLandConfidenceLevel', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TC_SVM] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_TC_SVM], $
               'SupportVectorSceneClassifier_1.1_km', numFields, Fields, $
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
                [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_TC_ASCM = (Icnt += 1)
Fields = ['AngularSignatureCloudMask','', '', '', '', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TC_ASCM] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_TC_ASCM], $
               'ASCMParams_1.1_km', numFields, Fields, $
                [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
                [512, 128, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; TC_ALBEDO data
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_ALB_22 = (Icnt += 1)
Fields = ['AlbedoLocal[0]','AlbedoLocal[1]', 'AlbedoLocal[2]','AlbedoLocal[3]', $
          'AlbedoLocalBroadband', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_22] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_ALB_22], $
               'ReflectingLevelParameters_2.2_km', numFields, Fields, $
               [-9999., -9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0], $
               [256, 64, 1], [0, 0, 0] }

; NOTE - regions need to be reduced in size by x 2 to accommodate block assembly
;        this is done when data are loaded
!KON.ProdTyp.PROD_ALB_352a = (Icnt += 1)
Fields = ['AlbedoRestrictive[0]', 'AlbedoRestrictive[1]', 'AlbedoRestrictive[2]', $
          'AlbedoRestrictive[3]', 'AlbedoRestrictiveBroadband', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_352a] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_ALB_352a], $
               'AlbedoParameters_35.2_km', numFields, Fields, $
               [-9999., -9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0], $
               [16, 4, 1], [0, 0, 0] }

; NOTE - regions need to be reduced in size by x 2 to accommodate block assembly
;        this is done when data are loaded
!KON.ProdTyp.PROD_ALB_352b = (Icnt += 1)
Fields = ['AlbedoExpansive[0]', 'AlbedoExpansive[1]', 'AlbedoExpansive[2]', $
          'AlbedoExpansive[3]', 'AlbedoExpansiveBroadband', '', '', '', '']
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_352b] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_ALB_352b], $
               'AlbedoParameters_35.2_km', numFields, Fields, $
               [-9999., -9999., -9999., -9999., -9999., 0.0, 0.0, 0.0, 0.0], $
               [16, 4, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; GRP_RCCM data
;----------------------------------------------------------------------------

!KON.ProdTyp.PROD_RCCM_MASK = (Icnt += 1)
Fields = ['Cloud', 'Glitter', '', '', '', '', '', '', ''] 
ndxs = WHERE(Fields NE '', numFields)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_RCCM_MASK] = $
             { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_RCCM_MASK], $
               'RCCM', numFields, Fields, $
               [255., 255., 255., 255., 255., 255., 255., 255., 255.], $
               [512, 128, 1], [0, 0, 0] }

;----------------------------------------------------------------------------
; GRP_ELLIPSOID and GRP_TERRAIN data. Items used only by Multi-Field Compare.
; The grid names, fill values and resolutions are not correct.
;----------------------------------------------------------------------------

Fields = ['Blue Radiance/RDQI', 'Green Radiance/RDQI', 'Red Radiance/RDQI', $
          'NIR Radiance/RDQI', '', '', '', '', ''] 
ndxs = WHERE(Fields NE '', numFields)

!KON.ProdTyp.PROD_ELL_GM = (Icnt += 1)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ELL_GM] = $
     { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_ELL_GM], $
       'dummy', numFields, Fields, $
       [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
       [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_ELL_LM = (Icnt += 1)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ELL_LM] = $
     { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_ELL_LM], $
       'dummy', numFields, Fields, $
       [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
       [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_TER_GM = (Icnt += 1)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TER_GM] = $
     { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_TER_GM], $
       'dummy', numFields, Fields, $
       [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
       [512, 128, 1], [0, 0, 0] }

!KON.ProdTyp.PROD_TER_LM = (Icnt += 1)
!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TER_LM] = $
     { product_struct, !KON.ProdTyp.ProdNames[!KON.ProdTyp.PROD_TER_LM], $
       'dummy', numFields, Fields, $
       [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], $
       [512, 128, 1], [0, 0, 0] }

Fields = 0

RETURN, 0

END  ;  InitMINXParams

;***************************************************************************
FUNCTION GetCoordStructMember
;***************************************************************************
   
COMPILE_OPT IDL2, LOGICAL_PREDICATE
   
axa = { $
   ; LonLat to SOM(xm,ym) parameters:
   PathNum      :         0, $   ;
   OrbitNum     :        0L, $   ;
   OrbitDate    :        '', $   ;
   OrbitLon     :      0.0D, $   ;
   OrbitPer     :      0.0D, $   ;
   OrbitInc     :      0.0D, $   ;
   ProjParms    :      !MAP, $   ;
   ; SOM(xm,ym) to MISR(b,l,s) parameters:
   DataField    :        '', $   ;
   DimCross     :         0, $   ;
   DimAlong     :         0, $   ;
   BlkBeg       :         1, $   ;
   BlkEnd       :       180, $   ;
   NumBlk       :       180, $   ;
   BlkPixOffset : PTR_NEW(), $   ;
   ULCcrossSOM  : PTR_NEW(), $   ;
   ULCalongSOM  : PTR_NEW(), $   ;
   LRCcrossSOM  : PTR_NEW(), $   ;
   LRCalongSOM  : PTR_NEW(), $   ;
   ; MISR(b,l,s) to Wndw(xpix,ypix) parameters:
   ULCwndwX     :         0, $   ;
   ULCwndwY     :       500, $   ;
   LRCwndwX     :      1000, $   ;
   LRCwndwY     :         0, $   ;
   ZoomFctr     :       1.0, $   ;
   add_edge     :         0, $   ;
   num_band     :         1, $   ;
   band_ndx     : [2, 1, 0, 3] $ ;
}

RETURN, axa
END  ;  GetCoordStructMember

;***************************************************************************
PRO CleanUpMemory, Retval
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON brf_data, BrfConvert_Fctrs, SolarIrradiance
COMMON agp_data, Land_Water_Mask, Terrain_Hts
COMMON gmp_data, Cam_Azimuth, Cam_Zenith, Cam_Scatter, Cam_Glitter, $
                 Sun_Azimuth, Sun_Zenith
COMMON svm_data, Cloud_Confid, Smoke_Confid, Dust_Confid, Land_Confid
COMMON aer_data, Tau_BE_Grid, Tau_LR_Grid, Angexp_BE_Grid, Angexp_LR_Grid, $
                 Ssa_Grid, Taufrac_Grid
COMMON lnd_data, BHR_Grid, DHR_Grid, NDVI_Grid, RPV_Grid
COMMON str_data, STER_ZeroHts, STER_CorrHts, STER_WndCrossLo, STER_WndAlongLo, $
                 STER_WndCrossHi, STER_WndAlongHi, STER_SDCM, Stereo_File
COMMON cld_data, CLDZ_ZeroHts, CLDC_CorrHts, CLD_MotionHts, CLDZ_WndCross, $
                 CLDC_WndCross, CLD_WndCross, CLD_WndAlong, CLDZ_CldMsk, $
                 CLDC_SDCM, CLD_CldMsk, Cloud_File
COMMON alb_data, Local_Albedo, Expans_Albedo, Restrict_Albedo
COMMON veg_data, Biome_grid, Biome_grid_spacing, Biome_swath, Biome_names

COMMON chan_minmax,   ChanMinMax
COMMON image_work,    WorkImage, ZOOM_WNDW, TLB, WORK_MinMax
COMMON blktimeangle,  BlockCntrTime, SomToSwathAngles
COMMON data_structs,  region_data, linept_data
COMMON struct_marker, marker_option
COMMON orblist_parm,  orblist_index
COMMON OpenWindows,   WndwStereoPlot, WndwStereoHist, WndwAerosolHist
COMMON save_product,  ProductList

Retval = 0

;---------------------------------------------------------------------------
; Free arrays and reset values in COMMON blocks.
;---------------------------------------------------------------------------

WorkImage        = 0
ChanMinMax       = 0
WORK_MinMax      = 0
;-----
BrfConvert_Fctrs = 0
SolarIrradiance  = 0
;-----
Land_Water_Mask  = 0
Terrain_Hts      = 0
;-----
Cam_Azimuth      = 0
Cam_Zenith       = 0
Cam_Scatter      = 0
Cam_Glitter      = 0
Sun_Azimuth      = 0
Sun_Zenith       = 0
;-----
Tau_BE_Grid      = 0
Tau_LR_Grid      = 0
Angexp_BE_Grid   = 0
Angexp_LR_Grid   = 0
Ssa_Grid         = 0
Taufrac_Grid     = 0
;-----
BHR_Grid         = 0
DHR_Grid         = 0
NDVI_Grid        = 0
RPV_Grid         = 0
Biome_grid       = 0
Biome_swath      = 0
Biome_names      = 0
;-----
STER_ZeroHts     = 0
STER_CorrHts     = 0
STER_WndCrossLo  = 0
STER_WndAlongLo  = 0
STER_WndCrossHi  = 0
STER_WndAlongHi  = 0
STER_SDCM        = 0
;-----
CLDZ_ZeroHts     = 0
CLDC_CorrHts     = 0
CLD_MotionHts    = 0
CLDZ_WndCross    = 0
CLDC_WndCross    = 0
CLD_WndCross     = 0
CLD_WndAlong     = 0
CLDZ_CldMsk      = 0
CLDC_SDCM        = 0
CLD_CldMsk       = 0
;-----
Cloud_Confid     = 0
Smoke_Confid     = 0
Dust_Confid      = 0
Land_Confid      = 0
;-----
Local_Albedo     = 0
Restrict_Albedo  = 0
Expans_Albedo    = 0
;-----
BlockCntrTime    = 0
SomToSwathAngles = 0
WndwStereoPlot   = 0
WndwStereoHist   = 0
WndwAerosolHist  = 0
;-----
region_data      = 0
linept_data      = 0
marker_option    = 0
orbitlist_index  = 0
;-----
ProductList      = 0

;---------------------------------------------------------------------------
; Set flags for parameters to indicate its memory no longer exists.
;---------------------------------------------------------------------------

!VAR.L1B2_TYPE = 'ELLIPSOID'

!VAR.CurrFiles.BRF_Loaded  = 0
!VAR.CurrFiles.AGP_Loaded  = 0
!VAR.CurrFiles.GMP_Loaded  = 0
!VAR.CurrFiles.AE1_Loaded  = 0
!VAR.CurrFiles.AE2_Loaded  = 0
!VAR.CurrFiles.LND_Loaded  = 0
!VAR.CurrFiles.STR_Loaded  = 0
!VAR.CurrFiles.CLD_Loaded  = 0
!VAR.CurrFiles.SVM_Loaded  = 0
!VAR.CurrFiles.ALB_Loaded  = 0
!VAR.CurrFiles.Fire_Loaded = 0
!VAR.CurrFiles.Biome_Loaded = 0
!VAR.CurrFiles.Marker_Loaded = 0
!VAR.CurrFiles.TerrHoles_Loaded = 0

!VAR.CurrFiles.BRFfile  = ''
!VAR.CurrFiles.AGPfile  = ''
!VAR.CurrFiles.GMPfile  = ''
!VAR.CurrFiles.AE1file  = ''
!VAR.CurrFiles.AE2file  = ''
!VAR.CurrFiles.LNDfile  = ''
!VAR.CurrFiles.STRfile  = ''
!VAR.CurrFiles.CLDfile  = ''
!VAR.CurrFiles.SVMfile  = ''
!VAR.CurrFiles.ALBfile  = ''
!VAR.CurrFiles.FireFile = ''
!VAR.CurrFiles.MarkerFile = ''
!VAR.CurrFiles.BiomeFile = ''

!VAR.CurrFiles.OverpassFile = ''
!VAR.CurrFiles.CamFiles[*]  = ''

!VAR.ClrScaleVals.ScalingMethod = 2
!VAR.ClrScaleVals.BandDisplayOpt = 1
!VAR.ClrScaleVals.Brightness = 0.5
!VAR.ClrScaleVals.Contrast = 0.5
!VAR.AutoWndw.INITIATE = 1

!VAR.DataRgn.SHOW = !KON.DataRgn.SHOW_DO_NOT
!VAR.DataRgn.POS_X = -1
!VAR.DataRgn.POS_Y = -1
!VAR.DataRgn.COLORKEY_DLG_ID  = -1
!VAR.DataRgn.COLORKEY_WNDW_ID = -1
!VAR.DataRgn.VALUE_MIN_ABS[*] =  9999.0
!VAR.DataRgn.VALUE_MAX_ABS[*] = -9999.0
!VAR.DataRgn.VALUE_MIN_DIG[*] =  9999.0
!VAR.DataRgn.VALUE_MAX_DIG[*] = -9999.0
!VAR.DataRgn.VALUE_MIN_DLG[*] =  9999.0
!VAR.DataRgn.VALUE_MAX_DLG[*] = -9999.0

!VAR.DataSwath.SHOW = !KON.DataRgn.SHOW_DO_NOT
!VAR.DataSwath.POS_X = -1
!VAR.DataSwath.POS_Y = -1
!VAR.DataSwath.OP_CHANGED = 1
!VAR.DataSwath.COLORKEY_DISC_OBJ = OBJ_NEW()
!VAR.DataSwath.COLORKEY_DISC_ID = -1
!VAR.DataSwath.COLORKEY_CONT_ID = -1
!VAR.DataSwath.COLORKEY_DLG_ID  = -1
!VAR.DataProd[*].MinData =  9999.0
!VAR.DataProd[*].MaxData = -9999.0
!VAR.DataProd[*].MinDlg  =  9999.0
!VAR.DataProd[*].MaxDlg  = -9999.0

;---------------------------------------------------------------------------
; Free the MINX images and digitized linked list elements in the heap.
; Don't delete the CoordStruct here.
;---------------------------------------------------------------------------

PTR_FREE, !VAR.RawImages[*,*]
!VAR.RawImages = PTR_NEW()
PTR_FREE, !VAR.NullPixelList[*,*]
!VAR.NullPixelList = PTR_NEW()
PTR_FREE, !VAR.NullValueList[*,*]
!VAR.NullValueList = PTR_NEW()
HEAP_FREE, !VAR.LinkList.pHeadObj
!VAR.LinkList.pHeadObj = PTR_NEW()
HEAP_GC, /VERBOSE

END  ;  CleanUpMemory
