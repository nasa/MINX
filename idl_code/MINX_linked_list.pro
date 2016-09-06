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

;************************************************************************
PRO LListDefineStructs
;************************************************************************
; Define the point and object structure types that are used in the object
; linked lists.
;------------------------------------------------------------------------

COMMON data_structs, region_data, linept_data

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Structure for data attached to an object. Some of these params have 2
; possible values: for red (0) and/or blue (1) retrievals.
;------------------------------------------------------------------------

region_data = { $
    type         :  0,  $       ; Type code for science object type
    band_type    :  0,  $       ; Type of band(s) to retrieve (== RETRIEVE_BAND_TYPE)
    name         : ['',''], $   ; Text to post for object (red & blue retrievals)
    color_pol    : '',  $       ; Color for polygon
    color_lin    : '',  $       ; Color for line
    symbol       : '',  $       ; Symbol to post for object
    samp_spac    : 0.0, $       ; Sample spacing for this object in km.
    show_line    :  0,  $       ; 1=draw line; 0=don't draw line
                                ;   connecting interior points
    utc_time     : '',  $       ; UTC time at first digitized point
    perimeter    : 0.0, $       ; Distance around perimeter in km.
    area         : 0.0, $       ; Area of region in sq.km.
    bestht_med   : [0.0,0.0], $ ; Best ~median ht for region (red & blue retrievals)
    bestht_top   : [0.0,0.0], $ ; Best ~top ht for region (red & blue retrievals)
    rms_ht_dev   : [0.0,0.0], $ ; RMS diff for raw - smoothed (red & blue retrievals)
    auto_quality : ['',''],   $ ; quality flag (red & blue retrievals)
    geo_region   : '',  $       ; Geographic region 1st point falls in.
    biome_type   : 255, $       ; IGBP biome classification code.
    biome_lat    : 0.0, $       ; Latitude/longitude for biome ID at largest value of
    biome_lon    : 0.0, $       ;   fire pixels or 1st point digitized if no fire pix.
    biome_elev   : 0.0, $       ; Elevation above MSL at biome_type location.
    red_blu_pref : '',  $       ; Red or Blue - whichever is judged better.
    user_pyrocum :  0,  $       ; User-supplied info - is pyro-cumulus present?
    user_comment : '',  $       ; User-supplied info - comment string

    direc_pts_orig : PTR_NEW(), $ ; Short list of input coordinates on direction
                                  ; line used only for writing to file.
    direc_pts_spln : PTR_NEW()  $ ; Long list of spline-fit coords on direction
                                  ; line used only for drawing direction arrow.
}

;------------------------------------------------------------------------
; Structure for data attached to each point ON a line or IN a region.
; The params immediately below have 2 possible values: for red (0) and
; blue (1) retrievals - either of both are possible.
;------------------------------------------------------------------------

band_ary    = INTARR(2)
cdispar_ary = FLTARR(!KON.Instr.NCAM,2) + !KON.Misc.BADVALUE_REAL
adispar_ary = FLTARR(!KON.Instr.NCAM,2) + !KON.Misc.BADVALUE_REAL
zwindht_ary = FLTARR(2)
cwindht_ary = FLTARR(2)
fwindht_ary = FLTARR(2)
cspeed_ary  = FLTARR(2)
aspeed_ary  = FLTARR(2)

modis_init = [-99.9, 0.0]

linept_data = { $
    type         :   0,  $      ; Geom type code for object (rgn,poly,line)
    feature_type :   0,  $      ; Feature type code for object
                                ;           (body,pyrocumulus,column,skirt)
    imagecross   :   0,  $      ; Window coords across
    imagealong   :   0,  $      ; Window coords along
    somcross     : 0.0,  $      ; Som coords across in km
    somalong     : 0.0,  $      ; Som coords along in km
    block        :   0,  $      ; 1-based block number
    cross275     :   0,  $      ; 0-based 275 meter pixels across
    along275     :   0,  $      ; 0-based 275 meter pixels along
    lat          : 0.0D, $      ; Latitude of point (dec deg)
    lon          : 0.0D, $      ; Longitude of point (dec deg)
    slope        : 0.0,  $      ; Plume slope relative to MISR grid.
    dirfromTruN  : 0.0,  $      ; Plume direction clockwise from true
                                ; north pointing away from source.
    dirfromSomN  : 0.0,  $      ; Plume direction clockwise from SOM
                                ; north pointing away from source.
    land_water   :   0,  $      ; Mask: land = , water = 
    terr_elev    : 0.0,  $      ; Terrain elevation (km AMSL)

    band_used    : band_ary,    $ ; Band used for this point: 0=red, 1=blue
    cross_dispar : cdispar_ary, $ ; Across disparity per camera.
    along_dispar : adispar_ary, $ ; Along disparity per camera.
    zero_wind_ht : zwindht_ary, $ ; Zero-wind smoke/cloud height above ellipsoid (km).
    corr_wind_ht : cwindht_ary, $ ; Wind-corrected smoke/cloud ht above ellipsoid (km).
    smooth_hghts : fwindht_ary, $ ; Zero-wind or wind-corrected filtered ht ASL (km).
    wnd_speed_cr : cspeed_ary,  $ ; Windspeed in cross-track direction.
    wnd_speed_al : aspeed_ary,  $ ; Windspeed in along-track direction.

    aer_tau      : FLTARR(!KON.Instr.NBAND) + !KON.Misc.BADVALUE_REAL, $
                                ; Optical depth per band from product.
    aer_ssa      : FLTARR(!KON.Instr.NBAND) + !KON.Misc.BADVALUE_REAL, $
                                ; Single-scatter albedo per band from product.
    aer_taufrac  : FLTARR(5) + !KON.Misc.BADVALUE_REAL, $
                                ; Tau fraction per particle type from product.
    aer_angexp   : -9.999, $    ; Angstrom exponent from product.
    modis_data   : FLTARR(2) + modis_init $  ; MODIS values are:
                                ; 0: power (MWatts),
                                ; 1: 0 if this fire pixel has not been used
                                ;    1 if this fire pixel has been used
}

band_ary    = 0
cdispar_ary = 0
adispar_ary = 0
zwindht_ary = 0
cwindht_ary = 0
fwindht_ary = 0
cspeed_ary  = 0
aspeed_ary  = 0

;------------------------------------------------------------------------
; Create base structures and initialize current object pointers.
; ObjType is an array concatenation of 4 defining parameters for a region:
; [!KON.AerObjTyp.x, !KON.GeomObjTyp.x, !KON.WindObjTyp.x, !KON.BandObjTyp.x]
; RtrvType is an array concatenation of 4 retrieval parameters for a region:
; 
;------------------------------------------------------------------------

LListObjType = {llist_obj_type, pNextSib    : PTR_NEW(), $
                                pNextPolyPt : PTR_NEW(), $
                                pNextLinePt : PTR_NEW(), $
                                pData       : PTR_NEW(), $
                                ObjType     : [-1, -1, -1, -1] }

LListPtType = {llist_pt_type, pNextSib : PTR_NEW(), $
                              pData    : PTR_NEW() } 

!VAR.LinkList.pHeadObj   = PTR_NEW()
!VAR.LinkList.pThisRgn   = PTR_NEW()
!VAR.LinkList.pHeadPoint = PTR_NEW()
!VAR.LinkList.pThisPoint = PTR_NEW()
!VAR.LinkList.PrevEventX = -1
!VAR.LinkList.PrevEventY = -1

END  ;  LListDefineStructs

;************************************************************************
FUNCTION InitRegionDataVals, Dummy
;************************************************************************
; Initialize data attached to an object. Some of these params have 2
; possible values: for red (0) and blue (1) retrievals. Make the structs
; variable in size for each case to avoid wasting memory.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

COMMON data_structs, region_data, linept_data

region_data.type            = 0
region_data.band_type       = !SAV.Digitize.RETRIEVE_BAND_TYPE
region_data.name[*]         = ''
region_data.color_pol       = ''
region_data.color_lin       = ''
region_data.symbol          = ''
region_data.samp_spac       = 0.0
region_data.show_line       = 0
region_data.utc_time        = ''
region_data.perimeter       = 0.0
region_data.area            = 0.0
region_data.bestht_med[*]   = 0.0
region_data.bestht_top[*]   = 0.0
region_data.rms_ht_dev[*]   = 0.0
region_data.auto_quality[*] = ''
region_data.geo_region      = ''
region_data.biome_type      = ''
region_data.red_blu_pref    = ''
region_data.user_pyrocum    = 0
region_data.user_comment    = ''

PTR_FREE, region_data.direc_pts_orig
region_data.direc_pts_orig = PTR_NEW()

PTR_FREE, region_data.direc_pts_spln
region_data.direc_pts_spln = PTR_NEW()

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   PTR_FREE, region_data.terrht_cntr
   PTR_FREE, region_data.terrht_hist
   region_data.tht_binsize = 0.0
   region_data.tht_numbins = 0
   region_data.terrht_cntr = PTR_NEW()
   region_data.terrht_hist = PTR_NEW()

   PTR_FREE, region_data.zeroht_cntr
   PTR_FREE, region_data.zeroht_hist
   region_data.zht_binsize = 0.0
   region_data.zht_numbins = 0
   region_data.zeroht_cntr = PTR_NEW()
   region_data.zeroht_hist = PTR_NEW()

   PTR_FREE, region_data.corrht_cntr
   PTR_FREE, region_data.corrht_hist
   region_data.cht_binsize = 0.0
   region_data.cht_numbins = 0
   region_data.corrht_cntr = PTR_NEW()
   region_data.corrht_hist = PTR_NEW()

   PTR_FREE, region_data.windx_cntr
   PTR_FREE, region_data.windx_hist
   region_data.wndx_binsize = 0.0
   region_data.wndx_numbins = 0
   region_data.windx_cntr   = PTR_NEW()
   region_data.windx_hist   = PTR_NEW()

   PTR_FREE, region_data.windy_cntr
   PTR_FREE, region_data.windy_hist
   region_data.wndy_binsize = 0.0
   region_data.wndy_numbins = 0
   region_data.windy_cntr   = PTR_NEW()
   region_data.windy_hist   = PTR_NEW()

   PTR_FREE, region_data.tau0_cntr
   PTR_FREE, region_data.tau0_hist
   region_data.tau0_binsize  = 0.0
   region_data.tau0_numbins  = 0
   region_data.tau0_cntr     = PTR_NEW()
   region_data.tau0_hist     = PTR_NEW()

   PTR_FREE, region_data.tau1_cntr
   PTR_FREE, region_data.tau1_hist
   region_data.tau1_binsize  = 0.0
   region_data.tau1_numbins  = 0
   region_data.tau1_cntr     = PTR_NEW()
   region_data.tau1_hist     = PTR_NEW()

   PTR_FREE, region_data.tau2_cntr
   PTR_FREE, region_data.tau2_hist
   region_data.tau2_binsize  = 0.0
   region_data.tau2_numbins  = 0
   region_data.tau2_cntr     = PTR_NEW()
   region_data.tau2_hist     = PTR_NEW()

   PTR_FREE, region_data.tau3_cntr
   PTR_FREE, region_data.tau3_hist
   region_data.tau3_binsize  = 0.0
   region_data.tau3_numbins  = 0
   region_data.tau3_cntr     = PTR_NEW()
   region_data.tau3_hist     = PTR_NEW()

   PTR_FREE, region_data.angexp_cntr
   PTR_FREE, region_data.angexp_hist
   region_data.ang_binsize  = 0.0
   region_data.ang_numbins  = 0
   region_data.angexp_cntr  = PTR_NEW()
   region_data.angexp_hist  = PTR_NEW()

   PTR_FREE, region_data.ssa0_cntr
   PTR_FREE, region_data.ssa0_hist
   region_data.ssa0_binsize  = 0.0
   region_data.ssa0_numbins  = 0
   region_data.ssa0_cntr     = PTR_NEW()
   region_data.ssa0_hist     = PTR_NEW()

   PTR_FREE, region_data.ssa1_cntr
   PTR_FREE, region_data.ssa1_hist
   region_data.ssa1_binsize  = 0.0
   region_data.ssa1_numbins  = 0
   region_data.ssa1_cntr     = PTR_NEW()
   region_data.ssa1_hist     = PTR_NEW()

   PTR_FREE, region_data.ssa2_cntr
   PTR_FREE, region_data.ssa2_hist
   region_data.ssa2_binsize  = 0.0
   region_data.ssa2_numbins  = 0
   region_data.ssa2_cntr     = PTR_NEW()
   region_data.ssa2_hist     = PTR_NEW()

   PTR_FREE, region_data.ssa3_cntr
   PTR_FREE, region_data.ssa3_hist
   region_data.ssa3_binsize  = 0.0
   region_data.ssa3_numbins  = 0
   region_data.ssa3_cntr     = PTR_NEW()
   region_data.ssa3_hist     = PTR_NEW()

   PTR_FREE, region_data.tfr0_cntr
   PTR_FREE, region_data.tfr0_hist
   region_data.tfr0_binsize  = 0.0
   region_data.tfr0_numbins  = 0
   region_data.tfr0_cntr     = PTR_NEW()
   region_data.tfr0_hist     = PTR_NEW()

   PTR_FREE, region_data.tfr1_cntr
   PTR_FREE, region_data.tfr1_hist
   region_data.tfr1_binsize  = 0.0
   region_data.tfr1_numbins  = 0
   region_data.tfr1_cntr     = PTR_NEW()
   region_data.tfr1_hist     = PTR_NEW()

   PTR_FREE, region_data.tfr2_cntr
   PTR_FREE, region_data.tfr2_hist
   region_data.tfr2_binsize  = 0.0
   region_data.tfr2_numbins  = 0
   region_data.tfr2_cntr     = PTR_NEW()
   region_data.tfr2_hist     = PTR_NEW()

   PTR_FREE, region_data.tfr3_cntr
   PTR_FREE, region_data.tfr3_hist
   region_data.tfr3_binsize  = 0.0
   region_data.tfr3_numbins  = 0
   region_data.tfr3_cntr     = PTR_NEW()
   region_data.tfr3_hist     = PTR_NEW()
ENDIF

RETURN, 1

END  ;  InitRegionDataVals

;************************************************************************
FUNCTION InitLinePtData, Dummy
;************************************************************************
; Initialize data attached to each point ON a line or IN a region.
; Some of these params have 2 possible values: for red (0) and blue (1)
; retrievals. Make the structs variable in size for each case to avoid
; wasting memory.
;------------------------------------------------------------------------

COMMON data_structs, region_data, linept_data

COMPILE_OPT IDL2, LOGICAL_PREDICATE

linept_data.type              = 0
linept_data.feature_type      = 0
linept_data.imagecross        = 0
linept_data.imagealong        = 0
linept_data.somcross          = 0.0
linept_data.somalong          = 0.0
linept_data.block             = 0
linept_data.cross275          = 0
linept_data.along275          = 0
linept_data.lat               = 0.0D
linept_data.lon               = 0.0D
linept_data.slope             = 0.0
linept_data.dirfromTruN       = 0.0
linept_data.dirfromSomN       = 0.0
linept_data.terr_elev         = 0.0

; These can have 1 or 2 values.
linept_data.band_used[*]      = 0.0
linept_data.cross_dispar[*,*] = !KON.Misc.BADVALUE_REAL
linept_data.along_dispar[*,*] = !KON.Misc.BADVALUE_REAL
linept_data.zero_wind_ht[*]   = 0.0
linept_data.corr_wind_ht[*]   = 0.0
linept_data.smooth_hghts[*]   = 0.0
linept_data.wnd_speed_cr[*]   = 0.0
linept_data.wnd_speed_al[*]   = 0.0

linept_data.aer_tau[*]     = !KON.Misc.BADVALUE_REAL
linept_data.aer_ssa[*]     = !KON.Misc.BADVALUE_REAL
linept_data.aer_taufrac[*] = !KON.Misc.BADVALUE_REAL
linept_data.modis_data     = [-99.9,0.0]

RETURN, 1

END  ;  InitLinePtData

;************************************************************************
FUNCTION LListNewNode, NodeType, Data
;************************************************************************
; Create a new linked list node of the requested type, insert data and
; return the pointer. Data is the data structure that will be placed in
; the new node.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (NodeType EQ !KON.NodeObjTyp.REGION_NODE) THEN tmp = {llist_obj_type}

IF (NodeType EQ !KON.NodeObjTyp.POLYPT_NODE OR $
    NodeType EQ !KON.NodeObjTyp.LINEPT_NODE) THEN $
   tmp = {llist_pt_type}

tmp.pData = PTR_NEW(Data)

(*(tmp.pData)).type = NodeType

RETURN, PTR_NEW(tmp)

END  ;  LListNewNode

;************************************************************************
FUNCTION LListGetTail, pLList
;************************************************************************
; Given a list head pointer, return a pointer to the tail (last) node of
; the list.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; See if node pointer is defined, else return an error.
;------------------------------------------------------------------------

IF (~ PTR_VALID(pLList)) THEN BEGIN
   MESSAGE, "Head pointer is undefined.", /CONTINUE
   RETURN, PTR_NEW()
ENDIF

;------------------------------------------------------------------------
; Get the next node and return it.
;------------------------------------------------------------------------

pNext = pLList
pPrev = PTR_NEW()

WHILE (PTR_VALID(pNext)) DO BEGIN
   pPrev = pNext
   pNext = (*pNext).pNextSib
ENDWHILE

RETURN, pPrev

END  ;  LListGetTail

;************************************************************************
FUNCTION LListInsertTail, pNode, NodeType, Data
;************************************************************************
; Insert a new node at the tail of the linked list. pNode is the head
; node pointer for the list. Do not use this to construct a large linked
; list from scratch; instead save the previous tail element and add to it.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; See if Data is defined, else return an error.
;------------------------------------------------------------------------

IF (N_ELEMENTS(Data) EQ 0) THEN BEGIN
   MESSAGE, "Data value undefined.", /CONTINUE
   RETURN, PTR_NEW()
ENDIF

;------------------------------------------------------------------------
; Create a new node.  If it is the first in this list, place its pointer
; in head location.  Otherwise, place it in the pNextSib position of the
; previous node.
;------------------------------------------------------------------------

tmp = LListNewNode(NodeType, Data)

IF (~ PTR_VALID(pNode)) THEN BEGIN
   pNode = tmp
ENDIF ELSE BEGIN
   pTail = LListGetTail(pNode)
   (*pTail).pNextSib = tmp
ENDELSE

RETURN, tmp

END  ;  LListInsertTail

;************************************************************************
FUNCTION LListNextNode, pNodeThis
;************************************************************************
; Given a node pointer, get the next node of the same type.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; See if node pointer is defined, else return an error.
;------------------------------------------------------------------------

IF (~ PTR_VALID(pNodeThis)) THEN BEGIN
   MESSAGE, "Node pointer is undefined.", /CONTINUE
   RETURN, PTR_NEW()
ENDIF

;------------------------------------------------------------------------
; Get the next node and return it.
;------------------------------------------------------------------------

RETURN, (*pNodeThis).pNextSib

END  ;  LListNextNode

;************************************************************************
FUNCTION LListPrevNode, pLList, pNodeThis
;************************************************************************
; Given a pointer to the head of a list and a pointer to a node in the
; list, return a pointer to the node before the passed node.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; See if node pointer is defined, else return an error.
;------------------------------------------------------------------------

IF (~ PTR_VALID(pLList) OR ~ PTR_VALID(pNodeThis)) THEN BEGIN
   MESSAGE, "A node pointer is undefined.", /CONTINUE
   RETURN, PTR_NEW()
ENDIF

;------------------------------------------------------------------------
; Loop over the nodes until the current node is found.  Return the node
; pointer before it.
;------------------------------------------------------------------------

pNext = pLList
pPrev = PTR_NEW()

WHILE (PTR_VALID(pNext)) DO BEGIN

   IF (pNext EQ pNodeThis) THEN RETURN, pPrev

   pPrev = pNext
   pNext = (*pNext).pNextSib

ENDWHILE

RETURN, PTR_NEW()

END  ;  LListPrevNode

;************************************************************************
FUNCTION LListCount, pLList
;************************************************************************
; Given a list head pointer, return the number of nodes in the list.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; See if node pointer is defined, else return an error.
;------------------------------------------------------------------------

IF (~ PTR_VALID(pLList)) THEN RETURN, 0

;------------------------------------------------------------------------
; Loop over the nodes counting as we go.
;------------------------------------------------------------------------

num_nodes = 0
pNext = pLList

WHILE (PTR_VALID(pNext)) DO BEGIN
   num_nodes += 1
   pNext = (*pNext).pNextSib
ENDWHILE

RETURN, num_nodes

END  ;  LList_nextnode

;************************************************************************
FUNCTION CompareObjects, StaticData, NodeData, Thresh
;************************************************************************
; Compare two object structures for identity. Thresh is a dummy variable
; here - it's required but not used.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Compare the names and HeadPt pointers to see if they are the same.
;------------------------------------------------------------------------

RetVal = -1

IF (StaticData.name[0] EQ NodeData.name[0] AND  $
    StaticData.HeadPt  EQ NodeData.HeadPt) THEN $
   RetVal = 0

;------------------------------------------------------------------------
; Return the result.
;------------------------------------------------------------------------

RETURN, RetVal

END  ;  CompareObjects

;************************************************************************
FUNCTION ComparePoints, StaticData, NodeData, ThreshDist
;************************************************************************
; Compare two point structures for identity.  Use the 275m coords as the
; basis for comparison, and allow a few pixels slop in the location,
; because the mouse click is not very accurate.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Compare the MISR block number and 275 meter along and across values for
; the search point and the current node point to see if they are within
; the threshold distance from each other for acceptance as being the same
; point.
;------------------------------------------------------------------------

cross_dist = LONG(StaticData[0] - NodeData.cross275)
along_dist = LONG(StaticData[1] - NodeData.along275)

dist = SQRT(along_dist * along_dist + cross_dist * cross_dist)

IF (StaticData[2] EQ NodeData.block AND dist LE ThreshDist) THEN $
   RETURN, 0

RETURN, -1

END  ;  ComparePoints

;************************************************************************
PRO LListDelDataRgn, DataStr
;************************************************************************
; Deallocate pointers and arrays in region data structure.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

PTR_FREE, DataStr.direc_pts_orig
PTR_FREE, DataStr.direc_pts_spln

IF (!KON.Misc.PRINT_HISTOGRAMS) THEN BEGIN
   PTR_FREE, DataStr.terrht_cntr
   PTR_FREE, DataStr.zeroht_cntr
   PTR_FREE, DataStr.corrht_cntr
   PTR_FREE, DataStr.windx_cntr
   PTR_FREE, DataStr.windy_cntr
   PTR_FREE, DataStr.tau0_cntr
   PTR_FREE, DataStr.tau1_cntr
   PTR_FREE, DataStr.tau2_cntr
   PTR_FREE, DataStr.tau3_cntr
   PTR_FREE, DataStr.angexp_cntr
   PTR_FREE, DataStr.ssa0_cntr
   PTR_FREE, DataStr.ssa1_cntr
   PTR_FREE, DataStr.ssa2_cntr
   PTR_FREE, DataStr.ssa3_cntr
   PTR_FREE, DataStr.tfr0_cntr
   PTR_FREE, DataStr.tfr1_cntr
   PTR_FREE, DataStr.tfr2_cntr
   PTR_FREE, DataStr.tfr3_cntr

   PTR_FREE, DataStr.terrht_hist
   PTR_FREE, DataStr.zeroht_hist
   PTR_FREE, DataStr.corrht_hist
   PTR_FREE, DataStr.windx_hist
   PTR_FREE, DataStr.windy_hist
   PTR_FREE, DataStr.tau0_hist
   PTR_FREE, DataStr.tau1_hist
   PTR_FREE, DataStr.tau2_hist
   PTR_FREE, DataStr.tau3_hist
   PTR_FREE, DataStr.angexp_hist
   PTR_FREE, DataStr.ssa0_hist
   PTR_FREE, DataStr.ssa1_hist
   PTR_FREE, DataStr.ssa2_hist
   PTR_FREE, DataStr.ssa3_hist
   PTR_FREE, DataStr.tfr0_hist
   PTR_FREE, DataStr.tfr1_hist
   PTR_FREE, DataStr.tfr2_hist
   PTR_FREE, DataStr.tfr3_hist
ENDIF

END  ;  LListDelDataRgn

;************************************************************************
PRO LListDelDataPolyPt, DataStr
;************************************************************************
; Deallocate pointers and arrays in polygon data structure.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

DataStr.cross_dispar = 0
DataStr.along_dispar = 0
DataStr.zero_wind_ht = 0
DataStr.corr_wind_ht = 0
DataStr.smooth_hghts = 0
DataStr.wnd_speed_al = 0
DataStr.wnd_speed_cr = 0
DataStr.aer_tau      = 0
DataStr.aer_ssa      = 0
DataStr.aer_taufrac  = 0
DataStr.modis_data   = 0

END  ;  LListDelDataPolyPt

;************************************************************************
PRO LListDelDataLinePt, DataStr
;************************************************************************
; Deallocate pointers and arrays in line data structure.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

DataStr.cross_dispar = 0
DataStr.along_dispar = 0
DataStr.zero_wind_ht = 0
DataStr.corr_wind_ht = 0
DataStr.smooth_hghts = 0
DataStr.wnd_speed_al = 0
DataStr.wnd_speed_cr = 0
DataStr.aer_tau      = 0
DataStr.aer_ssa      = 0
DataStr.aer_taufrac  = 0
DataStr.modis_data   = 0

END  ;  LListDelDataLinePt

;************************************************************************
PRO LListDelPoint, pLList, pDelNode
;************************************************************************
; Deletes the specified node and updates the pointers.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (~ PTR_VALID(pLList) OR ~ PTR_VALID(pDelNode) ) THEN BEGIN
    MESSAGE, "Pointer not valid for delete.", /CONTINUE
    RETURN
ENDIF

IF (pLList EQ pDelNode) THEN BEGIN
    MESSAGE, "You cannot delete the first point on a line.", /CONTINUE
    RETURN
ENDIF

;------------------------------------------------------------------------
; First get a pointer to the next and previous nodes.  If there is a
; previous sibling, update its next pointer.  If there is not, we're at
; the head structure, so update it.
;------------------------------------------------------------------------

pSavePrev = LListPrevNode(pLList, pDelNode)
pSaveNext = (*pDelNode).pNextSib

IF (PTR_VALID(pSavePrev)) THEN BEGIN
   (*pSavePrev).pNextSib = pSaveNext
ENDIF

;------------------------------------------------------------------------
; Delete the data in the object, then delete the data object and the
; selected node itself.
;------------------------------------------------------------------------

type = (*((*pDelNode).pData)).type

IF (type EQ !KON.NodeObjTyp.POLYPT_NODE) THEN LListDelDataPolyPt, $
                                               *((*pDelNode).pData)
IF (type EQ !KON.NodeObjTyp.LINEPT_NODE) THEN LListDelDataLinePt, $
                                               *((*pDelNode).pData)

PTR_FREE, (*pDelNode).pData
PTR_FREE, pDelNode

END  ;  LListDelPoint

;************************************************************************
PRO LListDelLineObj, pDelHeadPt, DelHeadPt
;************************************************************************
; This procedure deletes a single line linked list including all points
; it contains. If DelHeadPt is 0, then first point in list is not deleted.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

;------------------------------------------------------------------------
; Delete points in the object. Start at the point after head node for now.
;------------------------------------------------------------------------

IF (~ PTR_VALID(pDelHeadPt)) THEN RETURN

pNextNode = (*pDelHeadPt).pNextSib

WHILE (pNextNode) DO BEGIN
   pSave = (*pNextNode).pNextSib
   LListDelPoint, pDelHeadPt, pNextNode
   pNextNode = pSave
ENDWHILE

;------------------------------------------------------------------------
; Delete the data in the data object and the head node if requested.
;------------------------------------------------------------------------

IF (DelHeadPt) THEN BEGIN
   PTR_FREE, (*pDelHeadPt).pData
   PTR_FREE, pDelHeadPt
ENDIF ELSE BEGIN
   (*pDelHeadPt).pNextSib = PTR_NEW()
ENDELSE

END  ;  LListDelLineObj

;************************************************************************
PRO LListDelRgnObj, pDelObj
;************************************************************************
; This procedure deletes an entire region object including any linked
; lists and data it contains.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (~ PTR_VALID(pDelObj)) THEN RETURN

;------------------------------------------------------------------------
; First get a pointer to the next and previous objects.  If there is a
; previous sibling, update its next pointer.  If there is not, we're at
; the head structure, so update it.
;------------------------------------------------------------------------

pSavePrev = LListPrevNode(!VAR.LinkList.pHeadObj, pDelObj)
pSaveNext = (*pDelObj).pNextSib

IF (PTR_VALID(pSavePrev)) THEN BEGIN
   (*pSavePrev).pNextSib = pSaveNext
ENDIF ELSE BEGIN
   !VAR.LinkList.pHeadObj = pSaveNext
   IF (pDelObj EQ !VAR.LinkList.pThisRgn) THEN $
      !VAR.LinkList.pThisRgn = pSaveNext
ENDELSE

;------------------------------------------------------------------------
; Delete the polygon and line objects in the region.
;------------------------------------------------------------------------

LListDelLineObj, (*pDelObj).pNextPolyPt, 1
LListDelLineObj, (*pDelObj).pNextLinePt, 1

;------------------------------------------------------------------------
; Delete the data in the object then delete the data object and the
; selected node itself.
;------------------------------------------------------------------------

type = (*((*pDelObj).pData)).type

IF (type EQ !KON.NodeObjTyp.REGION_NODE) THEN LListDelDataRgn, $
                                               *((*pDelObj).pData)
PTR_FREE, (*pDelObj).pData
PTR_FREE, pDelObj
pDelObj = PTR_NEW()

END  ;  LListDelRgnObj

;************************************************************************
FUNCTION _LListSearch, pLList, CompareFunc, Data, pNode, Thresh
;************************************************************************
; Search the linked list for a match and return a null ptr if there is no
; match and a pointer if there is a match. Function is recursive.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE
FORWARD_FUNCTION _LListSearch

IF (~ PTR_VALID(pNode)) THEN RETURN, PTR_NEW()
IF (~ PTR_VALID((*pNode).pData)) THEN RETURN, PTR_NEW()

;------------------------------------------------------------------------
; See if we have a match.
;------------------------------------------------------------------------

cmp_res = CALL_FUNCTION(CompareFunc, Data, *((*pNode).pData), Thresh)

IF (cmp_res NE 0) THEN BEGIN
   RETURN, _LListSearch(pLList, CompareFunc, Data, (*pNode).pNextSib, $
                        Thresh)
ENDIF ELSE BEGIN
   RETURN, pNode
ENDELSE

END  ;  _LListSearch

;************************************************************************
FUNCTION LListSearch, pLList, CompareFunc, Data, Thresh
;************************************************************************
; Top level search the linked list for a match. Thresh is only used if
; the function is ComparePoints.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

IF (~ PTR_VALID(pLList)) THEN RETURN, PTR_NEW()
IF (~ PTR_VALID((*pLList).pData)) THEN RETURN, PTR_NEW()

cmp_res = CALL_FUNCTION(CompareFunc, Data, *((*pLList).pData), Thresh)
IF (cmp_res EQ 0) THEN RETURN, pLList

RETURN, _LListSearch(pLList, CompareFunc, Data, (*pLList).pNextSib, $
                     Thresh)

END  ;  LListSearch

;************************************************************************
PRO LListGetObjNames, TypeChars, Block, RgnCount, ObjNames
;************************************************************************
; Return an array containing the names of all regions of specified type
; in the specified block.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Loop over all the regions in the linked list starting at the head.
;------------------------------------------------------------------------

RgnCount = 0
pNext = !VAR.LinkList.pHeadObj

blknum = -999
typchars = ''

WHILE (pNext) DO BEGIN
   IF (PTR_VALID((*pNext).pData)) THEN BEGIN

      ;------------------------------------------------------------------
      ; Get the region name for the next existing region including the
      ; name of one of the members in a double-band node. Return the
      ; member name for the band that corresponds to the new band.
      ;------------------------------------------------------------------

      obj_name = (*((*pNext).pData)).name

      IF (obj_name[0] NE '') THEN BEGIN
         ipos = STRPOS(obj_name[0], '-B') + 2
         blknum = STRMID(obj_name[0], ipos, 3)
         ipos = STRPOS(blknum, '-')
         IF (ipos GT 0) THEN blknum = STRMID(blknum, 0, ipos)

         ipos = STRPOS(obj_name[0], '-', /REVERSE_SEARCH) + 1
         typchars = STRMID(obj_name[0], ipos, 3)
         
         obj_nam = obj_name[0]
      ENDIF ELSE BEGIN
         IF (obj_name[1] NE '') THEN BEGIN
            ipos = STRPOS(obj_name[1], '-B') + 2
            blknum = STRMID(obj_name[1], ipos, 3)
            ipos = STRPOS(blknum, '-')
            IF (ipos GT 0) THEN blknum = STRMID(blknum, 0, ipos)

            ipos = STRPOS(obj_name[1], '-', /REVERSE_SEARCH) + 1
            typchars = STRMID(obj_name[1], ipos, 3)
         
            obj_nam = obj_name[1]
         ENDIF
      ENDELSE
        
      IF (blknum EQ Block AND typchars EQ TypeChars) THEN BEGIN
         ObjNames[RgnCount] = obj_nam
         RgnCount += 1
      ENDIF
                   
      pNext = (*pNext).pNextSib
   ENDIF
ENDWHILE

;------------------------------------------------------------------------
; Eliminate duplicate names. 
;------------------------------------------------------------------------

ObjNames = ObjNames[UNIQ(ObjNames, SORT(ObjNames))]
ndxs = WHERE(ObjNames NE '', RgnCount)
ObjNames = ObjNames[ndxs]

type_chars = 0
Retval = 0

END  ;  LListGetObjNames

;************************************************************************
PRO LListObjTypeExist, ObjType, Exist
;************************************************************************
; Return 1 if there is an instance of the specified type; else 0.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Exist = 0

;------------------------------------------------------------------------
; Loop over all the regions in the linked list starting at the head.
;------------------------------------------------------------------------

pNext = !VAR.LinkList.pHeadObj

WHILE (pNext) DO BEGIN
   IF ((*pNext).ObjType[0] EQ ObjType[0] AND $
       (*pNext).ObjType[1] EQ ObjType[1] AND $
       (*pNext).ObjType[2] EQ ObjType[2] AND $
       (*pNext).ObjType[3] EQ ObjType[3]) THEN BEGIN
      Exist = 1
      RETURN
   ENDIF
   pNext = (*pNext).pNextSib
ENDWHILE

END  ;  LListObjTypeExist

;************************************************************************
PRO LListGetCoordArray, SizeY, pLList, Count, SomCoords, ImageCoords, $
                        MisrCoords, LonlatCoords
;************************************************************************
; Return an array containing the MISR coordinates for all points in the
; specified polygon or line.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Count the number of points to retrieve and allocate arrays of the
; appropriate size for the x and y coordinates.  We need both SOM coords
; (meters for whole swath) and image coords (pixel coords for whole
; multi-block image).
;------------------------------------------------------------------------

Count = LListCount(pLList)
IF (Count LT 1) THEN RETURN

LonlatCoords  = DBLARR(2,Count)
SomCoords     = FLTARR(2,Count)
MisrCoords    = INTARR(3,Count)
ImageCoords   = INTARR(2,Count)

;------------------------------------------------------------------------
; Retrieve the coordinates from each point node.
;------------------------------------------------------------------------

pNext = pLList
pt_cnt = 0

WHILE (pNext) DO BEGIN
   IF (PTR_VALID((*pNext).pData)) THEN BEGIN
      LonlatCoords[0,pt_cnt] = (*((*pNext).pData)).lon
      LonlatCoords[1,pt_cnt] = (*((*pNext).pData)).lat
      SomCoords[0,pt_cnt]    = (*((*pNext).pData)).somcross
      SomCoords[1,pt_cnt]    = (*((*pNext).pData)).somalong
      ImageCoords[0,pt_cnt]  = (*((*pNext).pData)).imagecross
      ImageCoords[1,pt_cnt]  = (*((*pNext).pData)).imagealong
      MisrCoords[0,pt_cnt]   = (*((*pNext).pData)).cross275
      MisrCoords[1,pt_cnt]   = (*((*pNext).pData)).along275
      MisrCoords[2,pt_cnt]   = (*((*pNext).pData)).block
      pNext = (*pNext).pNextSib
      pt_cnt += 1
   ENDIF
ENDWHILE

;------------------------------------------------------------------------
; Make sure the count matches.
;------------------------------------------------------------------------

IF (pt_cnt NE Count) THEN BEGIN
   LonlatCoords  = 0
   SomCoords     = 0
   MisrCoords    = 0
   ImageCoords   = 0
   RETURN
ENDIF

Retval = 0

END  ;  LListGetCoordArray

;************************************************************************
PRO LListGetRegionData, DataType, ArrElem, iBand, pLListPoly, $
                        CountPoly, ImagePolyCoords, pLListLine, $
                        CountLine, ImageLineCoords, DataVals
;************************************************************************
; Return an array containing the MISR coordinates for all points in the
; specified polygon or line. This is called only when drawing map colors.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Count the number of polygon points to retrieve and allocate an array of
; the appropriate size for the x and y coordinates.  We need only image
; coords (pixel coords for whole multi-block image).
;------------------------------------------------------------------------

CountPoly = LListCount(pLListPoly)
IF (CountPoly LT 1) THEN RETURN

ImagePolyCoords = INTARR(2,CountPoly)

;------------------------------------------------------------------------
; Retrieve the coordinates from each polygon point node.
;------------------------------------------------------------------------

pNext = pLListPoly
pt_cnt = 0

WHILE (pNext) DO BEGIN
   IF (PTR_VALID((*pNext).pData)) THEN BEGIN
      ImagePolyCoords[0,pt_cnt] = (*((*pNext).pData)).imagecross
      ImagePolyCoords[1,pt_cnt] = (*((*pNext).pData)).imagealong
      pNext = (*pNext).pNextSib
      pt_cnt += 1
   ENDIF
ENDWHILE

;------------------------------------------------------------------------
; Make sure the count matches.
;------------------------------------------------------------------------

IF (pt_cnt NE CountPoly) THEN BEGIN
   ImagePolyCoords = 0
   RETURN
ENDIF

;------------------------------------------------------------------------
; Count the number of line points to retrieve and allocate arrays of the
; appropriate size for the x and y coordinates.  We need both SOM coords
; (meters for whole swath) and image coords (pixel coords for whole
; multi-block image). The retrieval band specifies what we want to display.
;------------------------------------------------------------------------

CountLine = LListCount(pLListLine)
IF (CountLine LT 1) THEN RETURN
pNext = pLListLine
pt_cnt = 0

ImageLineCoords = INTARR(2,CountLine)

DataVals = FLTARR(CountLine)

WHILE (pNext) DO BEGIN
   pntr = (*pNext).pData
   IF (PTR_VALID(pntr)) THEN BEGIN
      ImageLineCoords[0,pt_cnt] = (*pntr).imagecross
      ImageLineCoords[1,pt_cnt] = (*pntr).imagealong

      CASE DataType OF
         !KON.DataRgn.TYPE_DISP_CROSS:  DataVals[pt_cnt] = $
                           (*pntr).cross_dispar[ArrElem,iBand]
         !KON.DataRgn.TYPE_DISP_ALONG:  DataVals[pt_cnt] = $
                           (*pntr).along_dispar[ArrElem,iBand]
         !KON.DataRgn.TYPE_ZEROWIND_HT: DataVals[pt_cnt] = $
                           (*pntr).zero_wind_ht[iBand]
         !KON.DataRgn.TYPE_WINDCORR_HT: DataVals[pt_cnt] = $
                           (*pntr).corr_wind_ht[iBand]
         !KON.DataRgn.TYPE_SMOOTHED_HT: DataVals[pt_cnt] = $
                           (*pntr).smooth_hghts[iBand]
         !KON.DataRgn.TYPE_WIND_CROSS:  DataVals[pt_cnt] = $
                           (*pntr).wnd_speed_cr[iBand]
         !KON.DataRgn.TYPE_WIND_ALONG:  DataVals[pt_cnt] = $
                           (*pntr).wnd_speed_al[iBand]
      ENDCASE

      pt_cnt += 1
      pNext = (*pNext).pNextSib
   ENDIF
ENDWHILE

DataVals = DataVals[0:pt_cnt-1]
ndxs = WHERE(DataVals GT !KON.Misc.BADVALUE_REAL, numndxs)
IF (numndxs EQ 0) THEN CountLine = 0L

Retval = 0

END  ;  LListGetRegionData

;************************************************************************
PRO LListGetPlumeSlopeDirec, pLList, Count, Slope, DirecSomN, $
                             DirecTruN, Retval
;************************************************************************
; Return arrays containing the MISR slope and direction for all points
; on the specified line.
;------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

Retval = -1

;------------------------------------------------------------------------
; Count the number of points to retrieve and allocate arrays of the
; appropriate size for the x and y coordinates.  We need both SOM coords
; (meters for whole swath) and image coords (pixel coords for whole
; multi-block image).
;------------------------------------------------------------------------

IF (Count LT 1) THEN RETURN

Slope     = FLTARR(Count)
DirecSomN = FLTARR(Count)
DirecTruN = FLTARR(Count)

;------------------------------------------------------------------------
; Retrieve the data from each point node.
;------------------------------------------------------------------------

pNext = pLList
pt_cnt = 0

WHILE (pNext) DO BEGIN
   IF (PTR_VALID((*pNext).pData)) THEN BEGIN
      Slope[pt_cnt] = (*((*pNext).pData)).slope
      DirecSomN[pt_cnt] = (*((*pNext).pData)).dirfromSomN
      DirecTruN[pt_cnt] = (*((*pNext).pData)).dirfromTruN
      pNext = (*pNext).pNextSib
      pt_cnt += 1
   ENDIF
ENDWHILE

;------------------------------------------------------------------------
; Make sure the count matches.
;------------------------------------------------------------------------

IF (pt_cnt NE Count) THEN BEGIN
    Slope     = 0
    DirecSomN = 0
    DirecTruN = 0
   RETURN
ENDIF

Retval = 0

END  ;  LListGetPlumeSlopeDirec
