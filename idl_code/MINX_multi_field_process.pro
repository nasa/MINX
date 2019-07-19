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
PRO DefineMISRDataFields
;***************************************************************************
; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
; CODE NEEDS TO BE REORGANIZED. 
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo

;------------------------------------------------------------------------
; Initialize array of structures containing info on MISR data fields to
; include. Don't do this again if already defined.
; IDL_DataType codes: byte = 1, int = 2, long = 3, float = 4, double = 5,
;                     unsigned int = 12
; FillValue is used only for fields that can take on negative values.
;------------------------------------------------------------------------

IF (N_ELEMENTS(MISR_FieldInfo) EQ 0) THEN BEGIN

   num_field_temp = 99

   FieldInst = { FieldStruct,  $
        FieldDialogName : '',  ProdGridName :  '', UnitsName    :  '', $
        ProdFieldName   : '',  ProductType  : '',  ImageType    :   0, $
        IDL_DataType    :   0, MinGoodVal   : 0.0, MaxGoodVal   : 0.0, $
        FillValue       : 0.0, CrossDim     :   0, AlongDim     :   0, $
        BandDim         :   0, CamsDim      :   0 }

   MISR_FieldInfo = REPLICATE(FieldInst, num_field_temp)

   ndx = -1
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Ellipsoid TOA BRF - global mode', '<>Band', $
        'TOA BRF', '<> Radiance/RDQI', 'GRP_ELLIPSOID_GM', $
        3, 12, 0.0, 65510.0, 65511., $
        !KON.Instr.HI_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Ellipsoid TOA BRF - local mode', '<>Band', $
        'TOA BRF', '<> Radiance/RDQI', 'GRP_ELLIPSOID_LM', $
        3, 12, 0.0, 65510.0, 65511., $
        !KON.Instr.HI_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Terrain TOA BRF - global mode',  '<>Band', $
        'TOA BRF', '<> Radiance/RDQI', 'GRP_TERRAIN_GM', $
        3, 12, 0.0, 65510.0, 65511., $
        !KON.Instr.HI_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Terrain TOA BRF - local mode',   '<>Band', $
        'TOA BRF', '<> Radiance/RDQI', 'GRP_TERRAIN_LM', $
        3, 12, 0.0, 65510.0, 65511., $
        !KON.Instr.HI_RES_PIX_CROSS, !KON.Instr.HI_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Solar zenith angle', 'GeometricParameters', $
        'Deg', 'SolarZenith', 'GP_GMP', $
        2, 5, 0.0, 90.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Solar azimuth angle', 'GeometricParameters', $
        'Deg', 'SolarAzimuth', 'GP_GMP', $
        2, 5, 0.0, 360.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Camera zenith angle', 'GeometricParameters', $
        'Deg', '{}Zenith', 'GP_GMP', $
        2, 5, 0.0, 90.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Camera azimuth angle', 'GeometricParameters', $
        'Deg', '{}Azimuth', 'GP_GMP', $
        2, 5, 0.0, 360.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Camera scatter angle', 'GeometricParameters', $
        'Deg', '{}Scatter', 'GP_GMP', $
        2, 5, 0.0, 360.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Camera glitter angle', 'GeometricParameters', $
        'Deg', '{}Glitter', 'GP_GMP', $
        2, 5, 0.0, 360.0, -999., 32, $
        8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'DEM Terrain height', 'Standard', $
        'Meters', 'AveSceneElev', 'AGP', $
        2, 2, -300.0, 10000.0, -9999., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Surface Feature Code', 'Standard', $
        'Code', 'SurfaceFeatureID', 'AGP', $
        1, 1, 0.0, 6.0, 0.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo height - zero-wind', 'SubregParams', $
        'Meters', 'PrelimERStereoHeight_WithoutWinds', 'TC_STEREO', $
        2, 2, -1000.0, 30000.0, -9999., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo height - wind-corrected', 'SubregParams', $
        'Meters', 'StereoHeight_BestWinds', 'TC_STEREO', $
        2, 2, -300.0, 30000.0, -9999., !KON.Instr.LO_RES_PIX_CROSS, $
        !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo Wind - NS low cloud speed', 'DomainParams', $
        'M/Sec', 'NSCloudMotionSpeedLowCloudBin', 'TC_STEREO', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo Wind - EW low cloud speed', 'DomainParams', $
        'M/Sec', 'EWCloudMotionSpeedLowCloudBin', 'TC_STEREO', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo Wind - NS high cloud speed', 'DomainParams', $
        'M/Sec', 'NSCloudMotionSpeedHighCloudBin', 'TC_STEREO', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo Wind - EW high cloud speed', 'DomainParams', $
        'M/Sec', 'EWCloudMotionSpeedHighCloudBin', 'TC_STEREO', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud height - zero-wind', 'Stereo_WithoutWindCorrection_1.1_km', $
        'Meters', 'CloudTopHeight_WithoutWindCorrection', 'TC_CLOUD', $
        2, 2, -1000.0, 30000.0, -9999., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud height - wind-corrected', 'Stereo_1.1_km', $
        'Meters', 'CloudTopHeight', 'TC_CLOUD', $
        2, 2, -300.0, 30000.0, -9999., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud height - from winds', 'Motion_17.6_km', $
        'Meters', 'CloudTopHeightOfMotion', 'TC_CLOUD', $
        2, 2, -300.0, 30000.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud Wind - northward speed', 'Motion_17.6_km', $
        'M/Sec', 'CloudMotionNorthward', 'TC_CLOUD', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud Wind - eastward speed', 'Motion_17.6_km', $
        'M/Sec', 'CloudMotionEastward', 'TC_CLOUD', $
        2, 4, -100.0, 100.0, -9999., $
        8, 2, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud Wind - zero-wind cross-track speed', $
        'Stereo_WithoutWindCorrection_1.1_km', $
        'M/Sec', 'CloudMotionCrossTrack_WithoutWindCorrection', 'TC_CLOUD', $
        2, 4, -200.0, 200.0, -22222., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol lowest residual optical depth', 'RegParamsPerMixture', $
        'AOD', 'RegLowestResidSpectralOptDepth', $
        'AS_AEROSOL', $
        2, 4, 0.0, 10.0, -9999., $
        32, 8, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol lowest residual angstrom exp', 'RegParamsPerMixture', $
        'Ang Exp', 'RegLowestResidAngstromExponent', 'AS_AEROSOL', $
        2, 4, -10.0, 10.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol lowest residual mixture', 'RegParamsPerMixture', $
        'Mix #', 'RegLowestResidMixture', $
        'AS_AEROSOL', 2, 12, 1.0, 252.0, 253., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol best est optical depth', 'RegParamsAer', $
        'AOD', 'RegBestEstimateSpectralOptDepth', 'AS_AEROSOL', $
        2, 4, 0.0, 10.0, -9999., $
        32, 8, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol best est angstrom exponent', 'RegParamsAer', $
        'Ang Exp', 'RegBestEstimateAngstromExponent', 'AS_AEROSOL', $
        2, 4, -10.0, 10.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol best est single scatter albedo', 'RegParamsAer', $
        'SS Alb', 'RegBestEstimateSpectralSSA', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol fraction small particles (green)', 'RegParamsAer', $
        'Fraction', 'RegBestEstimateSpectralOptDepthFraction[1][0]', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol fraction medium particles (green)', 'RegParamsAer', $
        'Fraction', 'RegBestEstimateSpectralOptDepthFraction[1][1]', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol fraction large particles (green)', 'RegParamsAer', $
        'Fraction', 'RegBestEstimateSpectralOptDepthFraction[1][2]', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol fraction spherical particles (green)', 'RegParamsAer', $
        'Fraction', 'RegBestEstimateSpectralOptDepthFraction[1][3]', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Aerosol fraction non-spherical particles (green)', 'RegParamsAer', $
        'Fraction', 'RegBestEstimateSpectralOptDepthFraction[1][4]', 'AS_AEROSOL', $
        2, 4, 0.0, 1.0, -9999., $
        32, 8, 1, 1}
;   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
;        'L2 Surface HDRF', 'SubregParamsLnd', $
;        'HDRF', 'LandHDRF', 'AS_LAND', $
;        2, 2, 0.0, 65532.0, 65533., $
;        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 4, 9}
;   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
;        'L2 Surface BRF', 'SubregParamsLnd', $
;        'BRF', 'LandBRF', 'AS_LAND', $
;        2, 2, 0.0, 65532.0, 65533., $
;        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 4, 9}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface BHR', 'SubregParamsLnd', $
        'BHR', 'LandBHR', 'AS_LAND', $
        2, 2, 0.0, 252.0, 253., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface DHR', 'SubregParamsLnd', $
        'DHR', 'LandDHR', 'AS_LAND', $
        2, 2, 0.0, 252.0, 253., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface NDVI', 'SubregParamsLnd', $
        'NDVI', 'NDVI', 'AS_LAND', $
        2, 2, -252.0, 252.0, 253., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface RPV-1 Green', 'SubregParamsLnd', $
        'RPV-1', 'BRFModParam1[1]', 'AS_LAND', $
        2, 2, 0.0, 65532.0, 65533., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface RPV-2 Green', 'SubregParamsLnd', $
        'RPV-2', 'BRFModParam2[1]', 'AS_LAND', $
        2, 1, -252.0, 252.0, 253., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Surface RPV-3 Green', 'SubregParamsLnd', $
        'RPV-3', 'BRFModParam3[1]', 'AS_LAND', $
        2, 1, -252.0, 252.0, 253., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Expansive albedo', 'AlbedoParameters_35.2_km', $
        'Albedo', 'AlbedoExpansive', 'TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        16, 4, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Restrictive albedo', 'AlbedoParameters_35.2_km', $
        'Albedo', 'AlbedoRestrictive', 'TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        16, 4, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Local albedo', 'ReflectingLevelParameters_2.2_km', $
        'Albedo', 'AlbedoLocal', 'TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        256, 64, 4, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Broadband expansive albedo', 'AlbedoParameters_35.2_km', $
        'Albedo', 'AlbedoExpansiveBroadband', 'TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        16, 4, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Broadband restrictive albedo', 'AlbedoParameters_35.2_km', $
        'Albedo', 'AlbedoRestrictiveBroadband','TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        16, 4, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'Broadband local albedo', 'ReflectingLevelParameters_2.2_km', $
        'Albedo', 'AlbedoLocalBroadband', 'TC_ALBEDO', $
        2, 4, 0.0, 10.0, -9999., $
        256, 64, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'SVM Smoke Mask', 'SupportVectorSceneClassifier', $
        'Code', 'SVMSmokeConfidenceLevel', 'TC_CLASSIFIERS', $
        1, 1, 0.0, 4.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'SVM Dust Mask', 'SupportVectorSceneClassifier', $
        'Code', 'SVMDustConfidenceLevel', 'TC_CLASSIFIERS', $
        1, 1, 0.0, 4.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'SVM Cloud Mask', 'SupportVectorSceneClassifier', $
        'Code', 'SVMCloudConfidenceLevel', 'TC_CLASSIFIERS', $
        1, 1, 0.0, 4.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'SVM Land Mask', 'SupportVectorSceneClassifier', $
        'Code', 'SVMLandConfidenceLevel', 'TC_CLASSIFIERS', $
        1, 1, 0.0, 4.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Radiometric cloud mask', 'RCCM', $
        'Code', 'Cloud', 'GRP_RCCM_GM', $
        1, 1, 0.0, 4.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L1 Radiometric glitter mask', 'RCCM', $
        'Code', 'Glitter', 'GRP_RCCM_GM', $
        1, 1, 0.0, 1.0, 255., $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo cloud mask - zero-wind-derived', 'SubregParams', $
        'Code', 'SDCM_WithoutWinds', 'TC_STEREO', $
        1, 1, 0.0, 4.0, 255.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Stereo cloud mask - wind-correction-derived', 'SubregParams', $
        'Code', 'SDCM_BestWinds', 'TC_STEREO', $
        1, 1, 0.0, 4.0, 255.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud cloud mask - zero-wind-derived', $
        'Stereo_WithoutWindCorrection_1.1_km', $
        'Code', 'StereoDerivedCloudMask_WithoutWindCorrection', 'TC_CLOUD', $
        1, 1, 0.0, 4.0, 255.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud cloud mask - wind-correction-derived', 'Stereo_1.1_km', $
        'Code', 'StereoDerivedCloudMask', 'TC_CLOUD', $
        1, 1, 0.0, 4.0, 255.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Cloud cloud mask - motion-derived', 'Motion_17.6_km', $
        'Code', 'MotionDerivedCloudMask', 'TC_CLOUD', $
        1, 1, 0.0, 4.0, 255.0, $
        32, 8, 1, 1}
   MISR_FieldInfo[(ndx+=1)] = { FieldStruct, $
        'L2 Angular signature cloud mask', 'ASCMParams_1.1_km', $
        'Code', 'AngularSignatureCloudMask', 'TC_CLASSIFIERS', $
        1, 1, 0.0, 4.0, 255.0, $
        !KON.Instr.LO_RES_PIX_CROSS, !KON.Instr.LO_RES_PIX_ALONG, 1, 1}

   MISR_FieldInfo = MISR_FieldInfo[0:ndx]

ENDIF

END  ;  DefineMISRDataFields

;***************************************************************************
PRO DefineMultiFieldStructs, MainWindowStruct, SubWndwStruct, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo
COMMON misr_setup_param, ParamStruct

MonthStr = ['January','February','March','April','May','June','July', $
            'August','September','October','November','December']

;------------------------------------------------------------------------
; Initialize a structure for storing parameters defining this multi-
; field session. Don't do this again if already defined.
;------------------------------------------------------------------------

IF (N_ELEMENTS(WindowInst) EQ 0) THEN BEGIN
   MainWindowStruct = { $
        PixelRes       : ParamStruct.pixel_res, $
        PathNum        : ParamStruct.path_number, $
        BlkOffsets     : LONARR(!KON.Instr.NUM_BLOCKS), $
        BegBlk         : ParamStruct.block_begin, $
        EndBlk         : ParamStruct.block_end, $
        CntrBlk        : ParamStruct.center_block, $
        CntrLine       : ParamStruct.center_line, $
        CntrSamp       : ParamStruct.center_samp, $
        RgnHeight      : ParamStruct.region_height, $
        RgnWidth       : ParamStruct.region_width, $
        NumSubWndw     : ParamStruct.num_list, $
        SubWndwBaseIDs : LONARR(ParamStruct.num_list), $
        MenuID         : 0, $
        HelpID         : 0, $
        ExitID         : 0, $
        ClkBlockNum    : 0, $
        Clk275Samp     : 0, $
        Clk275Line     : 0 }
ENDIF

;------------------------------------------------------------------------
; Initialize an array of structures containing parameters defining each
; sub-window of the session. Don't do this again if already defined.
;------------------------------------------------------------------------

IF (N_ELEMENTS(SubWindowInst) EQ 0) THEN BEGIN
   SubWindowInst = { SubWindowStruct,  $
        ProdFileName : '',  ProdGridName : '',  ProdFieldName : '', $
        FieldInfoNdx : 0,   CameraName   : '',  BandName      : '', $
        OrbitNumber  : 0L,  OrbitDate    : '',  ImageType     : 0,  $
        IDL_DataType : 0,   NativeRes    : 0.0, Title         : '', $
        MinValue     : 0.0, MaxValue     : 0.0, DescripVal    : '', $
        ScaleMinVal  : 0.0, ScaleMaxVal  : 0.0, TitleBarID    : 0,  $
        BlockID      : 0,   SampID       : 0,   LineID        : 0,  $
        DrawWndwID   : 0,   MinValueID   : 0,   MaxValueID    : 0,  $
        ScaleMinID   : 0,   ScaleMaxID   : 0,   DescripID     : 0,  $
        RedrawID     : 0,   DataValueID  : 0,   $
        DataArray    : PTR_NEW(), ImageArray : PTR_NEW() }
ENDIF

SubWndwStruct = REPLICATE(SubWindowInst, ParamStruct.num_list)

FOR iwndw=0,ParamStruct.num_list-1 DO BEGIN

   Status = -1

   band_used = 0
   BandName = ''
   CamName = ParamStruct.camera_list[iwndw]
   CamStr = ''
   IF (CamName NE '') THEN CamStr = CamName + ' - '

   ;---------------------------------------------------------------------
   ; If there is a band or camera component in the name of the data
   ; grid data field, then replace the templates <> or {} with the band
   ; or camera name.
   ;---------------------------------------------------------------------

   grid_name = $
        MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProdGridName
   npos = STRPOS(grid_name, '<>')
   IF (npos GE 0) THEN BEGIN
      toks = STRSPLIT(grid_name, '<>', /EXTRACT)
      GetBandDialog, $
         MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType, $
         '<Band> ' + toks[0], 0, BandNum, BandName, Cancel
      IF (Cancel NE 0) THEN RETURN
      
      grid_name = BandName + toks[0]
      band_used = 1
   ENDIF

   field_name = $
         MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProdFieldName
   npos = STRPOS(field_name, '<>')
   IF (npos GE 0) THEN BEGIN
      toks = STRSPLIT(field_name, '<>', /EXTRACT)
      IF (band_used EQ 0) THEN BEGIN
         GetBandDialog, $
            MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType,$
            '<Band> ' + toks[0], 0, BandNum, BandName, Cancel
         IF (Cancel NE 0) THEN RETURN
      
      ENDIF
      field_name = BandName + toks[0]
      title_str = '[' + CamName + ']'
      title_str += '[' + BandName + ']'
   ENDIF

   npos = STRPOS(field_name, '{}')
   IF (npos GE 0) THEN BEGIN
      toks = STRSPLIT(field_name, '{}', /EXTRACT)
      GetCamDialog, $
         MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType, $
         '<Camera> ' + toks[0], CamNum, CamName, Cancel
      IF (Cancel NE 0) THEN RETURN
      
      field_name = CamName + toks[0]
   ENDIF

   ;---------------------------------------------------------------------
   ; If the data field itself has a band or camera dimension, then add
   ; the correct dimension number to the field name.
   ;---------------------------------------------------------------------

   IF (MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].CamsDim GT 1 $
       AND $
       MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].BandDim GT 1) $
       THEN BEGIN
      GetCamBandDialog, $
         MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType, $
         field_name, CamNum, BandNum, CamName, BandName, Cancel
      IF (Cancel NE 0) THEN RETURN
      
      field_name += '[' + STRTRIM(STRING(BandNum),2) + ']' + $
                    '[' + STRTRIM(STRING(CamNum),2) + ']'
   ENDIF ELSE BEGIN
      IF (MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].CamsDim GT 1) $
          THEN BEGIN
         GetCamDialog, $
            MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType,$
            field_name, CamNum, CamName, Cancel
         IF (Cancel NE 0) THEN RETURN
         
         field_name += '[' + STRTRIM(STRING(CamNum),2) + ']'
      ENDIF
      IF (MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].BandDim GT 1) $
          THEN BEGIN
         GetBandDialog, $
            MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ProductType,$
            field_name, 1, BandNum, BandName, Cancel
         IF (Cancel NE 0) THEN RETURN
         
         field_name += '[' + STRTRIM(STRING(BandNum),2) + ']'
      ENDIF
   ENDELSE

   ;---------------------------------------------------------------------
   ; Construct the title of this data window.
   ;---------------------------------------------------------------------

   toks = STRSPLIT(ParamStruct.filenam_list[iwndw], '._', /EXTRACT, $
                   COUNT=numtoks)
   vers_name = toks[numtoks-2]
   IF (STRMID(toks[numtoks-2],0,1) EQ 'b' AND $
       STRLEN(toks[numtoks-2]) GE 4 AND $
       STRLEN(toks[numtoks-2]) LE 8) THEN BEGIN
      vers_name = toks[numtoks-3] + '.' + toks[numtoks-2]
   ENDIF

   title_str = ''
   IF (CamName  NE '') THEN title_str += '[' + CamName  + ']'
   IF (BandName NE '') THEN title_str += '[' + BandName + ']'

   wndw_name = $
        MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].FieldDialogName

   title = 'Orbit: ' + STRTRIM(STRING(ParamStruct.orbit_list[iwndw]),2) + $
           ' - Ver: ' + vers_name + ' - ' + wndw_name + ' ' + title_str

   ; If the title is too long to fit in the window's width, shorten the
   ; version name component.

   ntitlelen = STRLEN(title)
   IF (ntitlelen GT 84) THEN BEGIN
      nverslen = STRLEN(vers_name)
      vers_name = STRMID(vers_name, 0, 0 > (nverslen + 84 - ntitlelen))
      title = 'Orbit: ' + STRTRIM(STRING(ParamStruct.orbit_list[iwndw]),2) + $
              ' - Ver: ' + vers_name + ' - ' + wndw_name + ' ' + title_str
   ENDIF

   time_jul_gmt = -1.0
   Get_OrbitTime_OrbitNumber, time_jul_gmt, 0, ParamStruct.orbit_list[iwndw], $
                              90, status
   CALDAT, time_jul_gmt, month, day, year
   month = MonthStr[FIX(month-1)]
   DateString = month + ' ' + STRTRIM(STRING(day),2) + ', ' + $
                STRTRIM(STRING(year),2)

   ;---------------------------------------------------------------------
   ; Fill the data structure for this instance of the data window with
   ; the values that we know at this time.
   ;---------------------------------------------------------------------

   SubWndwStruct[iwndw] = { SubWindowStruct, $
        ParamStruct.filenam_list[iwndw], grid_name, field_name, $
        ParamStruct.fieldndx_list[iwndw], CamName, BandName, $
        ParamStruct.orbit_list[iwndw], DateString, $
        MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].ImageType, $
        MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].IDL_DataType, $
        !KON.Instr.HI_RES_PIX_CROSS / $
             MISR_FieldInfo[ParamStruct.fieldndx_list[iwndw]].CrossDim * $
            !KON.Instr.HI_RES_PIX_SIZE * 1000.0, $
        title, 0.0, 0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $
        PTR_NEW(), PTR_NEW() }
ENDFOR

Status = 0

END  ;  DefineMultiFieldStructs

;***************************************************************************
PRO TestForFileAvailability, Param_Struct, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_path, orig_path
COMMON misr_data_fields, MISR_FieldInfo

Status = -1

;------------------------------------------------------------------------
; Loop over all the requested data fields and search for the MISR
; product files that correspond with each. 
;------------------------------------------------------------------------

FOR ilist=0,Param_Struct.num_list-1 DO BEGIN

   ndx = WHERE(Param_Struct.field_list[ilist] EQ $
               MISR_FieldInfo[*].FieldDialogName, numndx)
   IF (numndx EQ 1) THEN BEGIN
      Param_Struct.fieldnam_list[ilist] = $
                            MISR_FieldInfo[ndx].ProdFieldName
      Param_Struct.fieldndx_list[ilist] = ndx
   ENDIF ELSE BEGIN
      mssg = 'No matching field name was found. ' + $
             'Consultant supplier of program.'
      rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   ENDELSE

ENDFOR

ndx = 0

;------------------------------------------------------------------------
; Search for the files corresponding to each requested product type.
; First find the unique product types to get.
;------------------------------------------------------------------------

path_str = STRTRIM(STRING(Param_Struct.path_number),2)
prod_type_list = STRARR(Param_Struct.num_list)
prod_ndx_list  = LONARR(Param_Struct.num_list)
num_prod_type = 0
save_beg_block =  9999
save_end_block = -9999

FOR ilist=0,Param_Struct.num_list-1 DO BEGIN

   orbit_str = STRTRIM(STRING(Param_Struct.orbit_list[ilist]),2)
   path_str  = STRTRIM(STRING(Param_Struct.path_number),2)
   IF (STRLEN(path_str) LT 3) THEN path_str = '0' + path_str
   IF (STRLEN(path_str) LT 3) THEN path_str = '0' + path_str
   path_str = 'P' + path_str
   prod_type = MISR_FieldInfo[Param_Struct.fieldndx_list[ilist]].ProductType
   CamName   = ''

   prod_type_list[num_prod_type] = prod_type
   prod_ndx_list[num_prod_type]  = ilist
   num_prod_type += 1

   ;---------------------------------------------------------------------
   ; If a new product was found, check on disk for the file. Ask user
   ; to specify band and camera if needed and to resolve other issues. 
   ;---------------------------------------------------------------------

   IF (prod_type EQ 'AGP') THEN BEGIN
      file_filter = ['MISR*' + prod_type + '*' + path_str + '*.hdf']
      prod_type_flag = !KON.FileTyp.TypeAgp
   ENDIF

   IF (prod_type EQ 'GRP_ELLIPSOID_GM' OR $
       prod_type EQ 'GRP_ELLIPSOID_LM' OR $
       prod_type EQ 'GRP_TERRAIN_GM'   OR $
       prod_type EQ 'GRP_TERRAIN_LM'   OR $
       prod_type EQ 'GRP_RCCM_GM'      OR $
       prod_type EQ 'GRP_RCCM_LM') THEN BEGIN

      field_name = $
         MISR_FieldInfo[Param_Struct.fieldndx_list[ilist]].ProdFieldName
      toks = STRSPLIT(field_name, '<>', /EXTRACT)
      GetCamDialog, $
         MISR_FieldInfo[Param_Struct.fieldndx_list[ilist]].ProductType, $
              '<Camera>' + toks[0], CamNum, CamName, Cancel
      IF (Cancel NE 0) THEN BEGIN
         mssg = ['File not valid for ' + $
                 Param_Struct.fieldnam_list[ilist], $
                 'for orbit number ' + orbit_str + '.', $
                 'Returning to the "Compare Data Products" dialog box.']
         RETURN
      ENDIF

      cam_name = STRUPCASE(CamName)
      file_filter = ['MISR*' + prod_type + '*' + path_str + '*' + $
                     orbit_str + '*' + cam_name + '*.hdf']
      IF (prod_type EQ 'GRP_ELLIPSOID_GM' OR $
          prod_type EQ 'GRP_ELLIPSOID_LM') THEN $
         prod_type_flag = !KON.FileTyp.TypeL1B2Ellipsoid
      IF (prod_type EQ 'GRP_TERRAIN_GM' OR $
          prod_type EQ 'GRP_TERRAIN_LM') THEN $
         prod_type_flag = !KON.FileTyp.TypeL1B2Terrain
      IF (prod_type EQ 'GRP_RCCM_GM' OR $
          prod_type EQ 'GRP_RCCM_LM') THEN $
         prod_type_flag = !KON.FileTyp.TypeL1B2RCCM
   ENDIF

   IF (prod_type EQ 'GP_GMP'    OR prod_type EQ 'TC_STEREO'      OR $
       prod_type EQ 'TC_CLOUD'  OR prod_type EQ 'TC_CLASSIFIERS' OR $
       prod_type EQ 'TC_ALBEDO' OR prod_type EQ 'AS_AEROSOL'     OR $
       prod_type EQ 'AS_LAND') THEN BEGIN ; OR prod_type EQ 'AS_AEROSOL new?'

      file_filter = ['MISR*' + prod_type + '*' + path_str + '*' + $
                     orbit_str + '*.hdf']
      IF (prod_type EQ 'GP_GMP')         THEN $
         prod_type_flag = !KON.FileTyp.TypeGpGmp
      IF (prod_type EQ 'TC_STEREO')      THEN $
         prod_type_flag = !KON.FileTyp.TypeStereo
      IF (prod_type EQ 'TC_CLOUD')      THEN $
         prod_type_flag = !KON.FileTyp.TypeCloud
      IF (prod_type EQ 'TC_CLASSIFIERS') THEN $
         prod_type_flag = !KON.FileTyp.TypeClass
      IF (prod_type EQ 'TC_ALBEDO')      THEN $
         prod_type_flag = !KON.FileTyp.TypeAlbedo
      IF (prod_type EQ 'AS_AEROSOL')     THEN $
            prod_type_flag = !KON.FileTyp.TypeAerosol
      IF (prod_type EQ 'AS_LAND')        THEN $
         prod_type_flag = !KON.FileTyp.TypeLand
   ENDIF

   ;---------------------------------------------------------------------
   ; Now ask the user to specify the filename. If a file can't be found,
   ; then either go back to the main dialog box, or remove all MISR data
   ; fields from the list that rely on the missing product.
   ;---------------------------------------------------------------------

   GetLastFilename, Param_Struct.path_number, prod_type_flag, $
                    file_filter, 1, file_outdir, full_outpath

   IF (~ FILE_TEST(full_outpath)) THEN BEGIN
      mssg = ['File not valid for ' + Param_Struct.fieldnam_list[ilist],$
              'for orbit number ' + orbit_str + '.', $
              'Returning to the "Compare Data Products" dialog box.']
      RETURN
   ENDIF ELSE BEGIN
      Param_Struct.camera_list[ilist]  = CamName
      Param_Struct.filenam_list[ilist] = full_outpath
   ENDELSE

   ;---------------------------------------------------------------------------
   ; Get the block range for this product. Reset the begin and end blocks that
   ; the used requested to be the widest range found for all the files. Don't
   ; include the AGP file, because it is often the entire orbit.
   ;---------------------------------------------------------------------------
   
   IF (prod_type NE 'AGP') THEN BEGIN
      GetFirstLastBlocks, full_outpath, beg_block, end_block, Retval
      
      IF (beg_block LT save_beg_block) THEN save_beg_block = beg_block
      IF (end_block GT save_end_block) THEN save_end_block = end_block
   ENDIF
ENDFOR

Param_Struct.block_begin = save_beg_block > Param_Struct.block_begin
Param_Struct.block_end   = save_end_block < Param_Struct.block_end

Status = 0

END  ;  TestForFileAvailability

;***************************************************************************
PRO LoadRadData, FileName, GridName, FieldName, BlkBeg, BlkEnd, Dims, $
                 DataAry, Status
;***************************************************************************
; Load the requested blocks of a L1 radiance file.
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; Set an error handler.
;---------------------------------------------------------------------------

CATCH, iErr
IF (iErr NE 0) THEN BEGIN
   mssg = ['There is a problem reading data from file:', FileName, $
           'Fix the problem and try running again.']
   rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
   CATCH, /CANCEL
   RETURN
ENDIF

;---------------------------------------------------------------------------
; If OpenShut is 1 or 3, open the file and the grid where the data field is
; located. Otherwise the file and grid must still be open for reading another
; field. If OpenShut is 2 or 3, close the file and the grid after use. If 
; OpenShut is 0, neither open or close the file - it is already open and
; stays open for reuse.
;---------------------------------------------------------------------------

FileId = EOS_GD_OPEN(FileName, /READ) 

IF (FileId LT 0) THEN BEGIN
   mssg = ['Error attempting to open file:', FileName]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   RETURN
ENDIF

GridId = EOS_GD_ATTACH(FileId, GridName)

IF (GridId LT 0) THEN BEGIN
   mssg = ['Error attempting to open grid: ', GridName]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   RETURN
ENDIF

;---------------------------------------------------------------------------
; Set the edge, start, stride.
;---------------------------------------------------------------------------

edges   = Dims
starts  = [0, 0, BlkBeg-1]
strides = [1, 1, 1]

;---------------------------------------------------------------------------
; Read all blocks of the data in one call for this data field.
;---------------------------------------------------------------------------

Status = EOS_GD_READFIELD(GridId, FieldName, DataAry, $
                          EDGE=edges, START=starts, STRIDE=strides) 
starts  = 0
strides = 0

IF (Status NE 0) THEN BEGIN
   mssg = ['Error attempting to read data from file/grid/field:', $
           'File : ' + FileName, 'Grid : ' + GridName, 'Field: ' + FieldName]
   rtrn = DIALOG_MESSAGE(mssg, /CENTER, /ERROR)
   CATCH, /CANCEL
   RETURN
ENDIF

;---------------------------------------------------------------------------
; If done reading fields, close the grid and the file. Cancel error handler.
;---------------------------------------------------------------------------

Status = EOS_GD_DETACH(GridId) 
Status = EOS_GD_CLOSE(FileId) 

CATCH, /CANCEL

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

CATCH, /CANCEL
Status = 0

END  ;  LoadRadData

;***************************************************************************
PRO UnScaleUnPackL1B2, ProdFileName, Gridname, BandNdx, BlkBeg, BlkEnd, $
                       Databuf
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Get the scale factor for this grid.
;------------------------------------------------------------------------

grid_id = EOS_GD_OPEN(ProdFileName)
gr_id   = EOS_GD_ATTACH(grid_id, Gridname)
status  = EOS_GD_READATTR(gr_id, 'Scale factor', attrval)
status  = EOS_GD_DETACH(gr_id)
status  = EOS_GD_CLOSE(grid_id)

;------------------------------------------------------------------------
; Apply the scale factor to the data.
;------------------------------------------------------------------------

Databuf = FLOAT(Databuf) * attrval[0]

;------------------------------------------------------------------------
; Get the factors for conversion to BRF. If data are old it might not
; contain BRF Conversion Factors. In that case create a 32x8 array
; containing values of 1.0.
;------------------------------------------------------------------------

num_blk = BlkEnd - BlkBeg + 1

BrfConversionFactors = GetBrfConversionFactors(ProdFileName, BandNdx, $
                                       num_blk, BlkBeg, AirMISR=0, 0)

IF (N_ELEMENTS(BrfConversionFactors) EQ 1) THEN BEGIN
   BrfConversionFactors = FLTARR(32,8,CoordStruct.(0).NumBlk) + 1.0
   IF (cam EQ 0) THEN BEGIN
      mssg = ['BRF Conversion Factors were not found.', $
              'You are probably using an older data version.', $
              'This orbit cannot be used.']
      rtrn = DIALOG_MESSAGE(mssg,/INFORMATION, /CENTER)
      Retval = -2
      RETURN
   ENDIF
ENDIF

;------------------------------------------------------------------------
; Apply the conversion factors to the data and return the result.
;------------------------------------------------------------------------

sizes = SIZE(Databuf)
temp_xs = sizes[1]
temp_ys = sizes[2]

FOR iblk = 0, num_blk-1 DO BEGIN
   Databuf[*,*,iblk] = TEMPORARY(Databuf[*,*,iblk]) * $
                       TEMPORARY(CONGRID(REFORM( $
                       BrfConversionFactors[*,*,iblk]),temp_xs,temp_ys))
ENDFOR

BrfConversionFactors = 0

END  ;  UnScaleUnPackL1B2

;***************************************************************************
PRO UnScaleUnPackLand, ProdFileName, Gridname, Fieldname, Databuf, $
                       NdxsGood, NumNdxsGood
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Get the scale factor and offset for this grid.
;------------------------------------------------------------------------

npos = STRPOS(Fieldname, '[')
field_name = STRMID(Fieldname, 0, npos)

grid_id = EOS_GD_OPEN(ProdFileName)
gr_id   = EOS_GD_ATTACH(grid_id, Gridname)
status  = EOS_GD_READATTR(gr_id, 'Scale '  + field_name, attrval1)
status  = EOS_GD_READATTR(gr_id, 'Offset ' + field_name, attrval2)
status  = EOS_GD_DETACH(gr_id)
status  = EOS_GD_CLOSE(grid_id)

;------------------------------------------------------------------------
; Apply the scale factor and offset to the data.
;------------------------------------------------------------------------

IF (NumNdxsGood GT 0) THEN BEGIN
   Databuf = FLOAT(Databuf)
   Databuf[NdxsGood] *= attrval1[0]
   Databuf[NdxsGood] += attrval2[0]
ENDIF

;------------------------------------------------------------------------
; A one-bit flag needs to be stripped from the BRF and HDRF fields.
;------------------------------------------------------------------------

IF (field_name EQ 'LandHDRF' OR field_name EQ 'LandBRF') THEN $
   Databuf[NdxsGood] /= 2.0

END  ;  UnScaleUnPackLand

;***************************************************************************
FUNCTION GetDescriptionFromCode, Field, Code
;***************************************************************************
; Convert the code number for the value at clicked point (values 1-4 or 1-6)
; into a description string.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo

Description = ''

IF (Field EQ 'Cloud' OR $
    Field EQ 'AngularSignatureCloudMask') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'Cloud high confidence'
   IF (Code EQ 2) THEN Description = 'Cloud low confidence'
   IF (Code EQ 3) THEN Description = 'Clear low confidence'
   IF (Code EQ 4) THEN Description = 'Clear high confidence'     
ENDIF

IF (Field EQ 'SDCM_WithoutWinds' OR $
    Field EQ 'SDCM_BestWinds' OR $
    Field EQ 'StereoDerivedCloudMask_WithoutWindCorrection' OR $
    Field EQ 'StereoDerivedCloudMask' OR $
    Field EQ 'MotionDerivedCloudMask') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'High confidence cloud'
   IF (Code EQ 2) THEN Description = 'Low confidence cloud'
   IF (Code EQ 3) THEN Description = 'Low confidence near surface'
   IF (Code EQ 4) THEN Description = 'High confidence near surface'
ENDIF

IF (Field EQ 'Glitter') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'Not glitter contaminated'
   IF (Code EQ 1) THEN Description = 'Glitter contaminated'
ENDIF

IF (Field EQ 'SurfaceFeatureID') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'Shallow ocean'
   IF (Code EQ 1) THEN Description = 'Land'
   IF (Code EQ 2) THEN Description = 'Coastline'
   IF (Code EQ 3) THEN Description = 'Shallow inland water'
   IF (Code EQ 4) THEN Description = 'Ephemeral water'
   IF (Code EQ 5) THEN Description = 'Deep inland water'
   IF (Code EQ 6) THEN Description = 'Deep ocean'
ENDIF

IF (Field EQ 'SVMSmokeConfidenceLevel') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'Smoke highly likely'
   IF (Code EQ 2) THEN Description = 'Smoke likely'
   IF (Code EQ 3) THEN Description = 'Smoke unlikely'
   IF (Code EQ 4) THEN Description = 'Smoke highly unlikely'
ENDIF

IF (Field EQ 'SVMDustConfidenceLevel') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'Dust highly likely'
   IF (Code EQ 2) THEN Description = 'Dust likely'
   IF (Code EQ 3) THEN Description = 'Dust unlikely'
   IF (Code EQ 4) THEN Description = 'Dust highly unlikely'
ENDIF

IF (Field EQ 'SVMCloudConfidenceLevel') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'Cloud highly likely'
   IF (Code EQ 2) THEN Description = 'Cloud likely'
   IF (Code EQ 3) THEN Description = 'Cloud unlikely'
   IF (Code EQ 4) THEN Description = 'Cloud highly unlikely'
ENDIF

IF (Field EQ 'SVMLandConfidenceLevel') THEN BEGIN
   IF (Code EQ 0) THEN Description = 'No retrieval'
   IF (Code EQ 1) THEN Description = 'Land highly likely'
   IF (Code EQ 2) THEN Description = 'Land likely'
   IF (Code EQ 3) THEN Description = 'Land unlikely'
   IF (Code EQ 4) THEN Description = 'Land highly unlikely'
ENDIF

RETURN, Description

END  ;  GetDescriptionFromCode

;***************************************************************************
PRO ReadMisrProductBlockRange, ProductFile, GridName, FieldNdx, FieldName, $
                               BlkBeg, BlkEnd, DataAry, Status
;***************************************************************************
; Read the requested block range for the requested product.
; THE ENTIRE SPECTRUM OF FILE-HANDLING CODE AND MISR PRODUCT DATA FIELD
; CODE NEEDS TO BE REORGANIZED. 
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo

Status = -1

;---------------------------------------------------------------------------
; Switch to the desired product to load.
;---------------------------------------------------------------------------

OpenShut  = [!KON.ProdTyp.OPEN_F, !KON.ProdTyp.SHUT_F]
Ifield    = -1
ProdType  = ''
Grid_Name = ''
HiBand = (GridName EQ 'RedBand')
HiCam  = STRPOS(ProductFile, 'AN') NE -1

CASE MISR_FieldInfo[FieldNdx].ProductType OF

   ;------------------------------------------------------------------------
   ; Load level 1 radiance data.
   ;------------------------------------------------------------------------

   'GRP_ELLIPSOID_GM' : BEGIN
      ProdType = 'ELL_GM'
      Grid_Name = GridName
      c_dim = (HiBand OR HiCam) ? 2048 : 512
      a_dim = (HiBand OR HiCam) ?  512 : 128
      Dims = [c_dim, a_dim, BlkEnd-BlkBeg+1]
   END

   'GRP_ELLIPSOID_LM' : BEGIN
      ProdType = 'ELL_LM'
      Grid_Name = GridName
      Dims = [2048, 512, BlkEnd-BlkBeg+1]
   END

   'GRP_TERRAIN_GM' : BEGIN
      ProdType = 'TER_GM'
      Grid_Name = GridName
      c_dim = (HiBand OR HiCam) ? 2048 : 512
      a_dim = (HiBand OR HiCam) ?  512 : 128
      Dims = [c_dim, a_dim, BlkEnd-BlkBeg+1]
   END

   'GRP_TERRAIN_LM' : BEGIN
      ProdType = 'TER_LM'
      Grid_Name = GridName
      Dims = [2048, 512, BlkEnd-BlkBeg+1]
   END

   ;------------------------------------------------------------------------
   ; Load AGP data.
   ;------------------------------------------------------------------------

   'AGP' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AGP].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN Ifield = ndx & ProdType = 'AGP'
   END

   ;------------------------------------------------------------------------
   ; Load GP_GMP data.
   ;------------------------------------------------------------------------

   'GP_GMP' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SUN].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'GPGMP_SUN' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_AZI].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'GPGMP_AZI' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_ZEN].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'GPGMP_ZEN' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_SCT].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'GPGMP_SCT' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_GPGMP_GLT].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'GPGMP_GLT' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load TC_STEREO data.
   ;------------------------------------------------------------------------

   'TC_STEREO' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_11].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'STER_11' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_STER_704].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'STER_704' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load TC_CLOUD data.
   ;------------------------------------------------------------------------

   'TC_CLOUD' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDZ_11].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'CLDZ_11' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLDC_11].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'CLDC_11' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_CLD_176].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'CLD_176' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load TC_CLASSIFIERS data.
   ;------------------------------------------------------------------------

   'TC_CLASSIFIERS' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TC_SVM].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'TC_SVM' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_TC_ASCM].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'TC_ASCM' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load TC_ALBEDO data.
   ;------------------------------------------------------------------------

   'TC_ALBEDO' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_22].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'ALB_22' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_352a].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'ALB_352a' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_ALB_352b].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'ALB_352b' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load AS_AEROSOL data.
   ;------------------------------------------------------------------------

   'AS_AEROSOL' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_TAU_BE].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_TAU_BE' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_SSA].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_SSA' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_AEX_BE].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_AEX_BE' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_FRC].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_FRC' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_TAU_LR].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_TAU_LR' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_AEX_LR].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_AEX_LR' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_MIX].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_MIX' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load AS_LAND data.
   ;------------------------------------------------------------------------

   'AS_LAND' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND1].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_LAND1' & ENDIF

      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_AS_LAND2].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'AS_LAND2' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Load RCCM data.
   ;------------------------------------------------------------------------

   'GRP_RCCM_GM' : BEGIN
      ndx = WHERE(!KON.ProdTyp.ProdParms[!KON.ProdTyp.PROD_RCCM_MASK].FieldNames EQ FieldName)
      IF (ndx GE 0) THEN BEGIN & Ifield = ndx & ProdType = 'RCCM_MASK' & ENDIF
   END

   ;------------------------------------------------------------------------
   ; Here otherwise.
   ;------------------------------------------------------------------------

   ELSE : BEGIN
   END

ENDCASE

;---------------------------------------------------------------------------
; If a field was matched, load the data.
;---------------------------------------------------------------------------

IF (ProdType NE '') THEN BEGIN
   IF (Grid_Name NE '') THEN BEGIN
      LoadRadData, ProductFile, Grid_Name, FieldName, BlkBeg, BlkEnd, Dims, $
                   DataAry, Status
   ENDIF ELSE BEGIN
      LoadHdfData, 0, OpenShut, 0, ProductFile, ProdType, Ifield[0], BlkBeg, $
                   BlkEnd, FileId, GridId, DataAry, Status
      IF (Status NE 0) THEN BEGIN
         mssg = 'LoadHdfData failed.'
         rval = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         RETURN
      ENDIF
   ENDELSE
ENDIF

END  ;  ReadMisrProductBlockRange

;***************************************************************************
PRO ReadMisrProductData, MainWindowStruct, SubWndwStruct, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo
COMMON misr_setup_param, ParamStruct

Status = -1

;------------------------------------------------------------------------
; Loop over the product fields.
;------------------------------------------------------------------------

nGoodWndw = 0

FOR iWndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN

   IF (iWndw GE MainWindowStruct.NumSubWndw) THEN BREAK
   BadValueFlag1 = 0.0
   BadValueFlag2 = MISR_FieldInfo[SubWndwStruct[iWndw].FieldInfoNdx].FillValue

   ;---------------------------------------------------------------------
   ; Read whole blocks without offsetting.
   ;---------------------------------------------------------------------

   num_blks = (MainWindowStruct.EndBlk - MainWindowStruct.BegBlk) + 1

   field_ndx = SubWndwStruct[iWndw].FieldInfoNdx

   ;---------------------------------------------------------------------
   ; Read the data. Handle the case where user requested RGB data,
   ; i.e. 3 fields of data to be displayed in the RGB channels.
   ;---------------------------------------------------------------------
   
   gridname  = STRARR(3)
   fieldname = STRARR(3)

   numband = 1
   gridname[0]  = SubWndwStruct[iWndw].ProdGridName
   fieldname[0] = SubWndwStruct[iWndw].ProdFieldName

   IF (STRMID(SubWndwStruct[iWndw].ProdGridName,0,3) EQ 'RGB') THEN BEGIN
      numband = 3
      bands = ['Red', 'Green', 'Blue']
      endgrid  = STRMID(SubWndwStruct[iWndw].ProdGridName, 3)
      endfield = STRMID(SubWndwStruct[iWndw].ProdFieldName, 3)
      FOR iband=0,numband-1 DO BEGIN
         gridname[iband]  = bands[iband] + endgrid
         fieldname[iband] = bands[iband] + endfield
      ENDFOR
   ENDIF

   FOR iband=0,numband-1 DO BEGIN

      ReadMisrProductBlockRange, SubWndwStruct[iWndw].ProdFileName, $
                                 gridname[iband], field_ndx, fieldname[iband], $
                                 FIX(MainWindowStruct.BegBlk, TYPE=2), $
                                 FIX(MainWindowStruct.EndBlk, TYPE=2), $
                                 databuf, rtrn

      ;------------------------------------------------------------------
      ; If either the gridname or fieldname wasn't found (maybe due to a
      ; version of the MISR file that is not supported), then remove the
      ; field from the data structure and continue.
      ;------------------------------------------------------------------

      IF (rtrn NE 0) THEN BEGIN
         mssg = ['Could not read field: ' + $
                 SubWndwStruct[iWndw].ProdFieldName, 'from file: ' + $
                 SubWndwStruct[iWndw].ProdFileName, $
                 'The MISR version of this file may not be supported ' + $
                 'by MINX.', 'Continuing without that field.']
         rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)
         IF (MainWindowStruct.NumSubWndw GT 1) THEN GOTO, remove_field
         CONTINUE
      ENDIF

      ;------------------------------------------------------------------
      ; Retrieve the native resolution of this field.
      ;------------------------------------------------------------------

      sizes = SIZE(databuf)
      SubWndwStruct[iWndw].NativeRes = $
             FLOAT(!KON.Instr.HI_RES_PIX_CROSS) * $
                   !KON.Instr.HI_RES_PIX_SIZE * 1000.0 / sizes[1]

      ;------------------------------------------------------------------
      ; Find all values that are out of valid range.
      ;------------------------------------------------------------------

      ndxs_bad = WHERE((databuf LT MISR_FieldInfo[field_ndx].MinGoodVal) OR $
                       (databuf GT MISR_FieldInfo[field_ndx].MaxGoodVal), $
                       numndxs_bad, COMPLEMENT=ndxs_good, $
                       NCOMPLEMENT=numndxs_good)

      ;------------------------------------------------------------------
      ; Process L1B2 radiance products separately.
      ;------------------------------------------------------------------

      IF (MISR_FieldInfo[field_ndx].ProductType EQ 'GRP_ELLIPSOID_GM' OR $
          MISR_FieldInfo[field_ndx].ProductType EQ 'GRP_ELLIPSOID_LM' OR $
          MISR_FieldInfo[field_ndx].ProductType EQ 'GRP_TERRAIN_GM'   OR $
          MISR_FieldInfo[field_ndx].ProductType EQ 'GRP_TERRAIN_LM') THEN BEGIN

         ;---------------------------------------------------------------
         ; Set the invalid entries to 0.
         ;---------------------------------------------------------------

         IF (numndxs_bad GT 0) THEN $
            databuf[ndxs_bad] = BadValueFlag1

         ;---------------------------------------------------------------
         ; Some level 1 products need to have their resolution decreased.
         ;---------------------------------------------------------------

         IF (SubWndwStruct[iWndw].CameraName EQ 'An' OR $
             STRMID(gridname[iband], 0, 3) EQ 'Red') THEN BEGIN

            databuf = TEMPORARY(REBIN(databuf, sizes[1] / 4, $
                                    sizes[2] / 4, num_blks, /SAMPLE))
         ENDIF

         ;---------------------------------------------------------------
         ; Remove the 2 least significant bits of GDQI. Don't worry
         ; about the bad GDQI values for now.
         ;---------------------------------------------------------------

         sizes = SIZE(databuf)
         databuf = TEMPORARY(ISHFT(databuf, -2))

         ;---------------------------------------------------------------
         ; Unscale and unpack L1 ellipsoid and terrain data.
         ;---------------------------------------------------------------

         UnScaleUnPackL1B2, SubWndwStruct[iWndw].ProdFileName, $
                            gridname[iband], iband, $
                            MainWindowStruct.BegBlk, $
                            MainWindowStruct.EndBlk, databuf

         ;---------------------------------------------------------------
         ; Concatenate the blocks.
         ;---------------------------------------------------------------

         databuf = REFORM(databuf, sizes[1], sizes[2] * num_blks, $
                          /OVERWRITE)

      ;------------------------------------------------------------------
      ; All other products either have the standard resolution or
      ; resolution needs to be increased. Concatenate these also.
      ;------------------------------------------------------------------

      ENDIF ELSE BEGIN

         ;---------------------------------------------------------------
         ; Set invalid entries to 0.0 if that is the minimum or to the
         ; field's fill value if the value can be less than 0. Pass that
         ; fill value to the display routine later.
         ;---------------------------------------------------------------

         IF (numndxs_bad GT 0) THEN BEGIN
            databuf[ndxs_bad] = $
               (MISR_FieldInfo[field_ndx].MinGoodVal GE 0.0) ? $
                               BadValueFlag1 : BadValueFlag2
         ENDIF

         ;---------------------------------------------------------------
         ; Some level 2 products need to have resolution increased.
         ;---------------------------------------------------------------

         old_pix = MISR_FieldInfo[SubWndwStruct[iWndw].FieldInfoNdx].CrossDim
         IF (old_pix LT 32) THEN old_pix = 32

         factor = FIX(LONG(!KON.Instr.HI_RES_PIX_CROSS) * $
                      LONG(!KON.Instr.HI_RES_PIX_SIZE * 1000.0) / $
                      MainWindowStruct.PixelRes) / old_pix

         IF (factor NE 1) THEN $
            databuf = TEMPORARY(REBIN(databuf, sizes[1] * factor, $
                                      sizes[2] * factor, /SAMPLE))

         ;---------------------------------------------------------------
         ; Unscale L2 TC_CLOUD wind data.
         ;---------------------------------------------------------------

         IF (MISR_FieldInfo[field_ndx].ProdFieldName EQ $
             'CloudMotionCrossTrack_WithoutWindCorrection') THEN BEGIN
            ndxs = WHERE(databuf GT BadValueFlag2, numndxs, $
                         COMPLEMENT=ndxsbad, NCOMPLEMENT=numbad)
            IF (numndxs GT 0) THEN databuf[ndxs] *= 0.01
         ENDIF

         ;---------------------------------------------------------------
         ; Unscale and unpack L2 AS_Land data.
         ;---------------------------------------------------------------

         IF (MISR_FieldInfo[field_ndx].ProductType EQ 'AS_LAND') THEN $
            UnScaleUnPackLand, SubWndwStruct[iWndw].ProdFileName, $
                               gridname[iband], fieldname[iband] + '[', $
                               databuf, ndxs_good, numndxs_good
      ENDELSE

      IF (numband EQ 3) THEN BEGIN
         IF (iband EQ 0) THEN BEGIN
            sizes = SIZE(databuf)
            data_buffer = MAKE_ARRAY(sizes[1], sizes[2], 3, $
                                     TYPE=sizes[3])
         ENDIF
         data_buffer[*,*,iband] = databuf
      ENDIF ELSE BEGIN
         data_buffer = databuf
      ENDELSE

   ENDFOR

   IF (N_ELEMENTS(data_buffer) EQ 0) THEN RETURN

   ;---------------------------------------------------------------------
   ; Store a pointer to the result in sub-window structure. Also
   ; store the minimum and maximum valid data values in structure.
   ;---------------------------------------------------------------------

   IF (MISR_FieldInfo[field_ndx].MinGoodVal GE 0.0 AND $
       (MISR_FieldInfo[field_ndx].ProdFieldName NE 'SVMSmokeConfidenceLevel' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'SVMDustConfidenceLevel' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'SVMCloudConfidenceLevel' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'SVMLandConfidenceLevel' AND $         MISR_FieldInfo[field_ndx].ProdFieldName NE 'Cloud' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'Glitter' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'SDCM_WithoutWinds' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'SDCM_BestWinds' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'StereoDerivedCloudMask_WithoutWindCorrection' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'StereoDerivedCloudMask' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'MotionDerivedCloudMask' AND $
        MISR_FieldInfo[field_ndx].ProdFieldName NE 'AngularSignatureCloudMask')) $
       THEN BEGIN
      ndxs = WHERE(data_buffer NE BadValueFlag1, numndxs)
   ENDIF ELSE BEGIN
      ndxs = WHERE(data_buffer NE BadValueFlag2, numndxs)
   ENDELSE

   IF (numndxs GT 0) THEN BEGIN
      min_val = MIN(data_buffer[ndxs], MAX=max_val)
      IF (min_val EQ max_val) THEN max_val = min_val + 1.0
      SubWndwStruct[iWndw].MinValue    = min_val
      SubWndwStruct[iWndw].MaxValue    = max_val
      SubWndwStruct[iWndw].ScaleMinVal = min_val
      SubWndwStruct[iWndw].ScaleMaxVal = max_val
      IF (MISR_FieldInfo[field_ndx].ImageType EQ 1) THEN $
         SubWndwStruct[iWndw].DescripVal = ''
   ENDIF ELSE BEGIN
      mssg = ['No data were found in the blocks you selected for field: ' + $
              SubWndwStruct[iWndw].ProdFieldName, 'from file: ' + $
              SubWndwStruct[iWndw].ProdFileName, $
              'Continuing without that field.']
      rtrn = DIALOG_MESSAGE(mssg, /ERROR, /CENTER)

remove_field:
      IF (MainWindowStruct.NumSubWndw-2 GE iWndw) THEN BEGIN
         SubWndwStruct[iWndw : MainWindowStruct.NumSubWndw-2] = $
            SubWndwStruct[iWndw+1 : MainWindowStruct.NumSubWndw-1] 
         ParamStruct.FieldNam_List[iWndw : MainWindowStruct.NumSubWndw-2] = $
            ParamStruct.FieldNam_List[iWndw+1 : MainWindowStruct.NumSubWndw-1]
         ParamStruct.FieldNdx_List[iWndw : MainWindowStruct.NumSubWndw-2] = $
            ParamStruct.FieldNdx_List[iWndw+1 : MainWindowStruct.NumSubWndw-1]
         ParamStruct.Field_List[iWndw : MainWindowStruct.NumSubWndw-2] = $
            ParamStruct.Field_List[iWndw+1 : MainWindowStruct.NumSubWndw-1]
         ParamStruct.FileNam_List[iWndw : MainWindowStruct.NumSubWndw-2] = $
            ParamStruct.FileNam_List[iWndw+1 : MainWindowStruct.NumSubWndw-1]
         ParamStruct.Orbit_List[iWndw : MainWindowStruct.NumSubWndw-2] = $
            ParamStruct.Orbit_List[iWndw+1 : MainWindowStruct.NumSubWndw-1]
      ENDIF

      iWndw -= 1
      ParamStruct.Num_List -= 1
      MainWindowStruct.NumSubWndw -= 1
      IF (MainWindowStruct.NumSubWndw GT 0) THEN BEGIN
         SubWndwStruct = SubWndwStruct[0:MainWindowStruct.NumSubWndw-1]
         ParamStruct.FieldNam_List = $
            ParamStruct.FieldNam_List[0:MainWindowStruct.NumSubWndw-1]
         ParamStruct.FieldNdx_List = $
            ParamStruct.FieldNdx_List[0:MainWindowStruct.NumSubWndw-1]
         ParamStruct.Field_List = $
            ParamStruct.Field_List[0:MainWindowStruct.NumSubWndw-1]
         ParamStruct.FileNam_List = $
            ParamStruct.FileNam_List[0:MainWindowStruct.NumSubWndw-1]
         ParamStruct.Orbit_List = $
            ParamStruct.Orbit_List[0:MainWindowStruct.NumSubWndw-1]
      ENDIF
      
      CONTINUE
   ENDELSE

   SubWndwStruct[iWndw].DataArray = PTR_NEW(FLOAT(data_buffer), /NO_COPY)

   nGoodWndw += 1

ENDFOR

ndxs = 0
ndxs_bad = 0
ndxs_good = 0
databuf = 0

IF (nGoodWndw LE 0) THEN RETURN

Status = 0

END  ;  ReadMisrProductData

;***************************************************************************
PRO DrawColorBar, Image, RR, GG, BB, ValMin, ValMax, BarTitle
;***************************************************************************
; Overlay a color scale bar on an existing image. Place it on the left
; edge of the image and near the top-to-bottom center.
;---------------------------------------------------------------------------

COMPILE_OPT idl2, LOGICAL_PREDICATE, STRICTARRSUBS

Status = -1

;--------------------------------------------------------------------------
; Get the size of the passed image.
;--------------------------------------------------------------------------

image_size = SIZE(image)
x_image_size = image_size[1]
y_image_size = image_size[2]

;--------------------------------------------------------------------------
; Set the color scale variables - sizes and colors.
;--------------------------------------------------------------------------

bar_width    = 20
bar_height   = 256
annot_width  = 36
title_height = 8
border_width = 4
text_color   = 16777215
bkgrnd_color = 0
annot_num    = 11

;--------------------------------------------------------------------------
; Compute other color scale dimensions, the coordinates at which it will
; be drawn and other parameters.
;--------------------------------------------------------------------------

bkgrnd_width  = bar_width  + annot_width  + border_width * 3.0
bkgrnd_height = bar_height + title_height + border_width * 3.0

xbeg_bkgrnd = border_width
xend_bkgrnd = xbeg_bkgrnd + bkgrnd_width
ybeg_bkgrnd = (y_image_size - bkgrnd_height) / 2.0  ; establishes vertical pos
yend_bkgrnd = ybeg_bkgrnd + bkgrnd_height

xbeg_bar = xbeg_bkgrnd + border_width
xend_bar = xbeg_bar + bar_width
ybeg_bar = ybeg_bkgrnd + border_width
yend_bar = ybeg_bar + bar_height

xmid_title = (xend_bkgrnd - xbeg_bkgrnd) / 2.0
xlft_annot = xend_bar + border_width
ybot_annot = ybeg_bar - border_width

;--------------------------------------------------------------------------
; Draw the passed image into a pixmap (virtual) window.
;--------------------------------------------------------------------------

WINDOW, /FREE, XSIZE=x_image_size, YSIZE=y_image_size, /PIXMAP
save_wndw = !D.WINDOW

TV, Image, /ORDER, TRUE=3

;--------------------------------------------------------------------------
; Draw a rectangle large enough to form a background for the color bar,
; title and scale annotations.
;--------------------------------------------------------------------------

POLYFILL, [xbeg_bkgrnd, xend_bkgrnd, xend_bkgrnd, xbeg_bkgrnd, xbeg_bkgrnd], $
          [ybeg_bkgrnd, ybeg_bkgrnd, yend_bkgrnd, yend_bkgrnd, ybeg_bkgrnd], $
          COLOR=bkgrnd_color, /DEVICE

;--------------------------------------------------------------------------
; Initialize plot parameters with a dummy line.
;--------------------------------------------------------------------------

PLOT, [xbeg_bar, xend_bar], [ybeg_bar, ybeg_bar], THICK=1, COLOR=0, $
      XRANGE=[0,x_image_size], YRANGE=[0,y_image_size], /DEVICE, /NOERASE, $
      POSITION=[0, 0, x_image_size, y_image_size], XSTYLE=5, YSTYLE=5

;--------------------------------------------------------------------------
; Draw the 256 colors of the color bar as 1-pixel thick lines.
;--------------------------------------------------------------------------

FOR icol=0,bar_height-1 DO BEGIN
   OPLOT, [xbeg_bar, xend_bar], [ybeg_bar+icol, ybeg_bar+icol], THICK=1, $
          COLOR=[RR[icol]+256*GG[icol]+(256^2)*BB[icol]]
ENDFOR

;--------------------------------------------------------------------------
; Draw a border around the color bar.
;--------------------------------------------------------------------------

OPLOT, [xbeg_bar, xend_bar, xend_bar, xbeg_bar, xbeg_bar], $
       [ybeg_bar, ybeg_bar, yend_bar, yend_bar, ybeg_bar], $
       THICK=2, COLOR=text_color

;--------------------------------------------------------------------------
; Set a smaller font, and write the title at the top.
;--------------------------------------------------------------------------

save_font_type = !P.FONT
!P.FONT = 0

;DEVICE, GET_FONTNAMES=font_names, SET_FONT='*'
;DEVICE, SET_FONT='screen12'

XYOUTS, xbeg_bkgrnd+2, yend_bar+6, BarTitle, ALIGNMENT=0.0, CHARSIZE=1, $
        COLOR=text_color, /DEVICE

;--------------------------------------------------------------------------
; Compute the annotation values and their y-locations beside color bar.
;--------------------------------------------------------------------------

annot_intrvl = (ValMax - ValMin) / (annot_num - 1.0)
ndec = FLOOR(ALOG10(annot_intrvl))
annot_intrvl = ROUND(annot_intrvl / (10.0 ^ ndec)) * (10.0 ^ ndec)

FOR iann=0,3 DO BEGIN
   annot_beg = (FLOOR(ValMin / annot_intrvl) + iann) * annot_intrvl
   IF (annot_beg GT ValMin) THEN BEGIN
      BREAK
   ENDIF
ENDFOR

FOR iann=0,3 DO BEGIN
   annot_end = (CEIL(ValMax / annot_intrvl) - iann) * annot_intrvl
   IF (annot_end LT ValMax) THEN BEGIN
      BREAK
   ENDIF
ENDFOR

annot_num = ROUND((annot_end - annot_beg) / annot_intrvl + 3)
yloc_intrvl = bar_height / (ValMax - ValMin)

annot_val = FLTARR(annot_num)
annot_val[0] = ValMin
FOR iann=1,annot_num-2 DO BEGIN
   annot_val[iann] = annot_beg + (iann - 1) * annot_intrvl
ENDFOR
annot_val[annot_num-1] = ValMax

annot_yloc = FLTARR(annot_num)
FOR iann=0,annot_num-1 DO BEGIN
   annot_yloc[iann] = ybot_annot + (annot_val[iann] - annot_val[0]) * $
                      yloc_intrvl
ENDFOR

;--------------------------------------------------------------------------
; Annotate the color bar values on the right.
;--------------------------------------------------------------------------

xlft = xlft_annot
xrgt = xlft + annot_width

ndec = FLOOR(ALOG10(ABS(annot_val[0]) > ABS(annot_val[annot_num-1])) > 0) + 1

IF (annot_val[0] LT 0.0 AND annot_val[annot_num-1] GT 0.0) THEN ndec += 1

FOR iann=0,annot_num-1 DO BEGIN
   IF (iann EQ 1 AND $
      (annot_val[1] - annot_val[0]) LT (annot_intrvl * 0.4)) THEN CONTINUE
   IF (iann EQ (annot_num - 2) AND $
      (annot_val[annot_num-1] - annot_val[annot_num-2]) LT $
      (annot_intrvl * 0.4)) THEN CONTINUE

   IF (ndec GT 3) THEN BEGIN
      fmt_str = '(I' + STRTRIM(STRING(ndec),2) + ')'
      data_val = STRTRIM(STRING(FORMAT=fmt_str, ROUND(annot_val[iann])),2)
   ENDIF ELSE BEGIN
      IF (ndec LT 0) THEN BEGIN
         fmt_str = '(F5.3)'
      ENDIF ELSE BEGIN
         IF (annot_val[iann] EQ FIX(annot_val[iann])) THEN BEGIN
            fmt_str = '(I5)'
         ENDIF ELSE BEGIN
            fmt_str = '(F5.' + STRTRIM(STRING(4-ndec),2) + ')'
         ENDELSE
      ENDELSE
      data_val = STRTRIM(STRING(FORMAT=fmt_str, annot_val[iann]),2)
   ENDELSE

   XYOUTS, xlft, annot_yloc[iann], data_val, COLOR=text_color, /DEVICE, $
           ALIGNMENT=0.0, CHARSIZE=0.6
ENDFOR

!P.FONT = save_font_type

;--------------------------------------------------------------------------
; Read the updated image from the pixmap window and destroy the window.
;--------------------------------------------------------------------------

Image = TVRD(0, 0, x_image_size, y_image_size, /ORDER, TRUE=3)

SafeWDELETE, save_wndw, didit

annot_val = 0

END  ;  DrawColorBar

;***************************************************************************
PRO MakeImageByte1, Values, AbsMax, MaxNum, OrbitImage, Status
;***************************************************************************
; Create an image when input data are byte values with fewer than 6 values.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Size the array to the desired output resolution.
;---------------------------------------------------------------------------

sizex = (SIZE(Values))[1]
sizey = (SIZE(Values))[2]

red_image   = BYTARR(sizex, sizey)
green_image = BYTARR(sizex, sizey)
blue_image  = BYTARR(sizex, sizey)

OrbitImage = BYTARR(sizex, sizey, 3)

;---------------------------------------------------------------------------
; Handle the AGP surface feature ID.
; 0 = Shallow Ocean, 1 = Land, 2 = Coastline, 3 = Shallow Inland Water
; 4 = Ephemeral Water, 5 = Deep Inland Water, 6 = Deep Ocean
;---------------------------------------------------------------------------

IF (AbsMax EQ 6) THEN BEGIN

;------------------------------------------------------------------------
; Set the codes for the feature IDs.
;------------------------------------------------------------------------

   ID_ShallowOcean       = 0
   ID_Land               = 1
   ID_Coastline          = 2
   ID_ShallowInlandWater = 3
   ID_EphemeralWater     = 4
   ID_DeepInlandWater    = 5
   ID_DeepOcean          = 6
   ID_None               = 255

   ndx = WHERE(Values EQ ID_ShallowOcean, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 127
      green_image[ndx] = 127
      blue_image[ndx]  = 255
   ENDIF

   ndx = WHERE(Values EQ ID_Land, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 128
      green_image[ndx] = 96
      blue_image[ndx]  = 16
   ENDIF

   ndx = WHERE(Values EQ ID_Coastline, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 255
      green_image[ndx] = 0
      blue_image[ndx]  = 0
   ENDIF

   ndx = WHERE(Values EQ ID_ShallowInlandWater, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 127
      green_image[ndx] = 255
      blue_image[ndx]  = 127
   ENDIF

   ndx = WHERE(Values EQ ID_EphemeralWater, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 115
      green_image[ndx] = 176
      blue_image[ndx]  = 59
   ENDIF

   ndx = WHERE(Values EQ ID_DeepInlandWater, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 0
      green_image[ndx] = 255
      blue_image[ndx]  = 0
   ENDIF

   ndx = WHERE(Values EQ ID_DeepOcean, numndx)
   IF (numndx GT 0) THEN BEGIN
      red_image[ndx]   = 0
      green_image[ndx] = 0
      blue_image[ndx]  = 255
   ENDIF

   GOTO, skiphere
ENDIF

;---------------------------------------------------------------------------
; Set the codes for the confidence levels.
;---------------------------------------------------------------------------

REGTYPE_HC    = 1
REGTYPE_LC    = 2
NO_REGTYPE_LC = 3
NO_REGTYPE_HC = 4

;---------------------------------------------------------------------------
; Set the colors in the image array.
;---------------------------------------------------------------------------

ndx = WHERE(Values EQ 0, numndx)
IF (numndx GT 0) THEN BEGIN
   red_image[ndx]   = 0
   green_image[ndx] = 0
   blue_image[ndx]  = 0
ENDIF

IF (MaxNum EQ 0) THEN GOTO, skiphere
ndx = WHERE(Values EQ REGTYPE_HC, numndx)
IF (numndx GT 0) THEN BEGIN
   red_image[ndx]   = 0
   green_image[ndx] = 255
   blue_image[ndx]  = 0
ENDIF

IF (MaxNum EQ REGTYPE_HC) THEN GOTO, skiphere
ndx = WHERE(Values EQ REGTYPE_LC, numndx)
IF (numndx GT 0) THEN BEGIN
   red_image[ndx]   = 255 / 2
   green_image[ndx] = 255
   blue_image[ndx]  = 255 / 2
ENDIF

IF (MaxNum EQ REGTYPE_LC) THEN GOTO, skiphere
ndx = WHERE(Values EQ NO_REGTYPE_LC, numndx)
IF (numndx GT 0) THEN BEGIN
   red_image[ndx]   = 255
   green_image[ndx] = 255 / 2
   blue_image[ndx]  = 255 / 2
ENDIF

IF (MaxNum EQ NO_REGTYPE_LC) THEN GOTO, skiphere
ndx = WHERE(Values EQ NO_REGTYPE_HC, numndx)
IF (numndx GT 0) THEN BEGIN
   red_image[ndx]   = 255
   green_image[ndx] = 0
   blue_image[ndx]  = 0
ENDIF

skiphere:

ndx = 0
OrbitImage[*,*,0] = red_image
OrbitImage[*,*,1] = green_image
OrbitImage[*,*,2] = blue_image

red_image = 0
green_image = 0
blue_image = 0

Status = 1

END ; MakeImageByte1

;***************************************************************************
PRO MakeImageByte2, Values, ValueMin, ValueMax, BadVal, OrbitImage, $
                    BarTitle, Status
;***************************************************************************
; Create a 256-color image for single-band data.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

Status = -1

;---------------------------------------------------------------------------
; Compute the index into the color table for each pixel.
;---------------------------------------------------------------------------

ndxgood = WHERE(Values NE BadVal, numgood)
IF (numgood LT 2) THEN RETURN

coeff = (!D.TABLE_SIZE-1) / (ValueMax - ValueMin)
clrndx = FIX(coeff * (Values[ndxgood] - ValueMin))

;---------------------------------------------------------------------------
; Create RGB arrays to combine into a 3-band image and scale the data into
; RGB pixel values.
;---------------------------------------------------------------------------

sizex = (SIZE(Values))[1]
sizey = (SIZE(Values))[2]

red_image   = BYTARR(sizex, sizey)
green_image = BYTARR(sizex, sizey)
blue_image  = BYTARR(sizex, sizey)

OrbitImage = BYTARR(sizex, sizey, 3)

red_image[ndxgood]   = BYTSCL(R_CURR[clrndx],MIN=0,MAX=!D.TABLE_SIZE-1)
green_image[ndxgood] = BYTSCL(G_CURR[clrndx],MIN=0,MAX=!D.TABLE_SIZE-1)
blue_image[ndxgood]  = BYTSCL(B_CURR[clrndx],MIN=0,MAX=!D.TABLE_SIZE-1)

;---------------------------------------------------------------------------
; Combine the 3 single color images to form the RGB image.
;---------------------------------------------------------------------------

OrbitImage[*,*,0] = red_image
OrbitImage[*,*,1] = green_image
OrbitImage[*,*,2] = blue_image

;---------------------------------------------------------------------------
; Draw a scale bar on the left edge of image.
;---------------------------------------------------------------------------

DrawColorBar, OrbitImage, R_CURR, G_CURR, B_CURR, ValueMin, ValueMax, BarTitle

;---------------------------------------------------------------------------
; Clean up.
;---------------------------------------------------------------------------

ndxgood = 0
red_image = 0
green_image = 0
blue_image = 0

Status = 1

END ; MakeImageByte2

;***************************************************************************
PRO MakeImageInt, Values, Minval, Maxval, OrbitImage, Status
;***************************************************************************
; Create an image when the input data are 3 RGB bands of integers.
; This is trivial.

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Status = -1

;---------------------------------------------------------------------------
; Handle both single band and RGB images.
;---------------------------------------------------------------------------

OrbitImage = BYTSCL(Values, MIN=Minval, MAX=Maxval)

Status = 1

END ; MakeImageInt

;***************************************************************************
PRO CreateDataImages, MainWindowStruct, SubWndwStruct, Min, Max, Status
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON misr_data_fields, MISR_FieldInfo

;------------------------------------------------------------------------
; Get the index of the selected data field.
;------------------------------------------------------------------------

field_ndx = SubWndwStruct.FieldInfoNdx

;------------------------------------------------------------------------
; Delete the old image if it exists.
;------------------------------------------------------------------------

IF (PTR_VALID(SubWndwStruct.ImageArray)) THEN BEGIN
   PTR_FREE, SubWndwStruct.ImageArray
   SubWndwStruct.ImageArray = PTR_NEW() 
ENDIF

;------------------------------------------------------------------------
; Determine the data type and create the image.
;------------------------------------------------------------------------

IF (MISR_FieldInfo[field_ndx].ImageType EQ 1) THEN $
   MakeImageByte1, *SubWndwStruct.DataArray, $
                   MISR_FieldInfo[SubWndwStruct.FieldInfoNdx].MaxGoodVal, $
                   Max, OrbitImage, status

IF (MISR_FieldInfo[field_ndx].ImageType EQ 2) THEN BEGIN
   BadVal = MISR_FieldInfo[SubWndwStruct.FieldInfoNdx].FillValue
   MakeImageByte2, *SubWndwStruct.DataArray, Min, Max, BadVal, OrbitImage, $
                   MISR_FieldInfo[field_ndx].UnitsName, status
ENDIF

IF (MISR_FieldInfo[field_ndx].ImageType EQ 3) THEN $
   MakeImageInt, *SubWndwStruct.DataArray, Min, Max, OrbitImage, status

IF (MISR_FieldInfo[field_ndx].ImageType GT 3) THEN RETURN

;------------------------------------------------------------------------
; Store the image in the structure in the main window.
;------------------------------------------------------------------------

SubWndwStruct.ImageArray = PTR_NEW(OrbitImage, /NO_COPY)

END  ;  CreateDataImages

;***************************************************************************
PRO MultiFieldCompare, OrigPath, Cancel
;***************************************************************************

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON save_path, orig_path
COMMON misr_data_fields, MISR_FieldInfo
COMMON misr_setup_param, ParamStruct

orig_path = OrigPath

;------------------------------------------------------------------------
; Create the structure containing the information on MISR data fields
; that this option can display.
;------------------------------------------------------------------------

DefineMISRDataFields

;------------------------------------------------------------------------
; Initialize setup values.
;------------------------------------------------------------------------

MaxSubWndw = 20

param_struct = { pixel_res     : !KON.Instr.LO_RES_PIX_SIZE * 1000.0, $
                 num_list      : 0, $
                 orbit_list    : LONARR(MaxSubWndw), $
                 filenam_list  : STRARR(MaxSubWndw), $
                 field_list    : STRARR(MaxSubWndw), $
                 fieldnam_list : STRARR(MaxSubWndw), $
                 fieldndx_list : LONARR(MaxSubWndw), $
                 camera_list   : STRARR(MaxSubWndw), $
                 path_number   : 0,   $
                 region_type   : 1,   $
                 block_begin   : 1,  $
                 block_end     : 180, $
                 center_block  : 90,  $
                 center_line   : 64,  $
                 center_samp   : 256, $
                 region_height : 128, $
                 region_width  : 256, $
                 user_cancel   : 0 }

;------------------------------------------------------------------------
; Call the dialog box code.
;------------------------------------------------------------------------

redo_dialog:
HELP, /STRUCTURE, ParamStruct, OUTPUT=exists
is_struct = STRTRIM(exists[1],2)

IF (~ STRMATCH(is_struct, 'UNDEFINED = <Undefined>') EQ 1) THEN $
    param_struct = ParamStruct

PrepMultiFieldDialog, MaxSubWndw, PixelRes, MISR_FieldInfo.FieldDialogName, $
                      param_struct, Cancel

IF (Cancel EQ 1) THEN BEGIN
   OrigPath = orig_path
   RETURN
ENDIF

ParamStruct = param_struct
param_struct = 0

;------------------------------------------------------------------------
; Define and fill the structures that contain main window and sub-
; window properties.
;------------------------------------------------------------------------

DefineMultiFieldStructs, MainWindowStruct, SubWndwStruct, Status
IF (Status NE 0) THEN GOTO, redo_dialog

;------------------------------------------------------------------------
; Get the requested data from the MISR products.
;------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS
ReadMisrProductData, MainWindowStruct, SubWndwStruct, Status

IF (Status NE 0) THEN GOTO, redo_dialog

;------------------------------------------------------------------------
; Create the data images to display in the windows.
;------------------------------------------------------------------------

FOR iwndw=0,MainWindowStruct.NumSubWndw-1 DO BEGIN
   temp_struct = SubWndwStruct[iwndw]
   field_ndx = SubWndwStruct[iwndw].FieldInfoNdx

   CreateDataImages, MainWindowStruct, temp_struct, $
                     temp_struct.MinValue, $
                     temp_struct.MaxValue, $
                     Status
   SubWndwStruct[iwndw] = temp_struct
ENDFOR

;------------------------------------------------------------------------
; Display the data.
;------------------------------------------------------------------------

WIDGET_CONTROL, /HOURGLASS
MultiFieldDataWndw, MainWindowStruct, SubWndwStruct, Status

OrigPath = orig_path

END  ;  MultiFieldCompare
