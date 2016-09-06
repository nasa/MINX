;===========================================================================
;                                                                          =
;                                     MINX                                 =
;                                                                          =
;===========================================================================
;                                                                          =
;                           Jet Propulsion Laboratory                      =
;                                     MISR                                 =
;                                                                          =
;           Copyright 2007-2015, California Institute of Technology.       =
;                             ALL RIGHTS RESERVED.                         =
;                   U.S. Government Sponsorship acknowledged.              =
;                                                                          =
;===========================================================================

;***************************************************************************
PRO GetPathLatLonCoords, Path, BlkBeg, BlkEnd, WrapLon, LatLonCoords
;***************************************************************************
; Define the Lat-Lon coordinates of the corners of all 180 MISR blocks. Every
; MISR path has identical latitude block corner coordinates, so they are
; needed just once. Every MISR path has identical longitude block corner
; coordinates shifted by a constant number of degrees, so they are needed
; just once also. The values below are for Path 1.
; WrapLon: wrap longitude values by adding 360 when they reach this negative
; value. NOTE - the coordinates represent the actual outside edges of the
; blocks, not the centers of any pixels near the edges.
;---------------------------------------------------------------------------

COMPILE_OPT idl2, LOGICAL_PREDICATE, STRICTARRSUBS

;---------------------------------------------------------------------------
; Table of geographic coordinates of block corners.
;---------------------------------------------------------------------------

;     Lon UL      Lat UL      Lon LL       Lat LL       Lon LR      Lat LR       Lon UR      Lat UR
;   ---------    --------    ---------    --------     --------    --------     --------    --------

LLCornerCoords = $                                                                                        ; BLOCK
[[ 110.46694D,  66.22267D,  109.70868D,  67.44818D,   97.72107D,  65.75270D,   98.96263D,  64.60402D], $  ;   1
 [ 109.70868D,  67.44818D,  108.87162D,  68.66982D,   96.36174D,  66.88971D,   97.72107D,  65.75270D], $
 [ 108.45364D,  68.62920D,  107.50145D,  69.84414D,   94.49679D,  67.93947D,   96.00308D,  66.81941D], $
 [ 107.50145D,  69.84414D,  106.43652D,  71.05346D,   92.83601D,  69.04363D,   94.49679D,  67.93947D], $
 [ 105.97354D,  71.00705D,  104.74640D,  72.20681D,   90.60425D,  70.04657D,   92.45488D,  68.96513D], $
 [ 104.74640D,  72.20681D,  103.35272D,  73.39806D,   88.54967D,  71.10595D,   90.60425D,  70.04657D], $
 [ 103.35272D,  73.39806D,  101.75555D,  74.57890D,   86.26028D,  72.13939D,   88.54967D,  71.10595D], $
 [ 101.75555D,  74.57890D,   99.90687D,  75.74681D,   83.70048D,  73.14216D,   86.26028D,  72.13939D], $
 [  99.32042D,  75.68469D,   97.11698D,  76.83110D,   80.40091D,  74.00392D,   83.27830D,  73.04351D], $
 [  97.11698D,  76.83110D,   94.51122D,  77.95565D,   77.17150D,  74.92042D,   80.40091D,  74.00392D], $  ;  10
 [  94.51122D,  77.95565D,   91.39092D,  79.05167D,   73.54361D,  75.78454D,   77.17150D,  74.92042D], $
 [  91.39092D,  79.05167D,   87.60567D,  80.10972D,   69.47101D,  76.58619D,   73.54361D,  75.78454D], $
 [  87.60567D,  80.10972D,   82.95560D,  81.11630D,   64.91293D,  77.31355D,   69.47101D,  76.58619D], $
 [  82.17525D,  81.01578D,   76.38849D,  81.93992D,   59.49251D,  77.81390D,   64.52666D,  77.18116D], $
 [  76.38849D,  81.93992D,   69.21459D,  82.76375D,   53.95950D,  78.34483D,   59.49251D,  77.81390D], $
 [  69.21459D,  82.76375D,   60.39235D,  83.44937D,   47.96381D,  78.75954D,   53.95950D,  78.34483D], $
 [  60.39235D,  83.44937D,   49.83705D,  83.94964D,   41.58684D,  79.04484D,   47.96381D,  78.75954D], $
 [  49.83705D,  83.94964D,   37.87241D,  84.21631D,   34.95663D,  79.19049D,   41.58684D,  79.04484D], $
 [  37.87241D,  84.21631D,   25.34934D,  84.21697D,   28.23718D,  79.19085D,   34.95663D,  79.19049D], $
 [  25.34934D,  84.21697D,   13.37950D,  83.95154D,   21.60611D,  79.04590D,   28.23718D,  79.19085D], $  ;  20
 [  13.37950D,  83.95154D,    2.81611D,  83.45230D,   15.22757D,  78.76126D,   21.60611D,  79.04590D], $
 [   2.81611D,  83.45230D,   -6.01462D,  82.76747D,    9.22977D,  78.34715D,   15.22757D,  78.76126D], $
 [  -6.01462D,  82.76747D,  -13.19599D,  81.94421D,    3.69438D,  77.81675D,    9.22977D,  78.34715D], $
 [ -13.19599D,  81.94421D,  -18.98882D,  81.02049D,   -1.34223D,  77.18447D,    3.69438D,  77.81675D], $
 [ -18.98882D,  81.02049D,  -23.67762D,  80.02413D,   -5.87980D,  76.46457D,   -1.34223D,  77.18447D], $
 [ -23.67762D,  80.02413D,  -27.50815D,  78.97476D,   -9.94254D,  75.67022D,   -5.87980D,  76.46457D], $
 [ -27.50815D,  78.97476D,  -30.67437D,  77.88611D,  -13.56849D,  74.81306D,   -9.94254D,  75.67022D], $
 [ -31.33627D,  77.96108D,  -33.94424D,  76.83665D,  -17.22496D,  74.00851D,  -13.99372D,  74.92477D], $
 [ -33.94424D,  76.83665D,  -36.14942D,  75.69033D,  -20.10396D,  73.04830D,  -17.22496D,  74.00851D], $
 [ -36.14942D,  75.69033D,  -38.03589D,  74.52694D,  -22.67441D,  72.05116D,  -20.10396D,  73.04830D], $  ;  30
 [ -38.03589D,  74.52694D,  -39.66706D,  73.34999D,  -24.97586D,  71.02288D,  -22.67441D,  72.05116D], $
 [ -40.18438D,  73.40385D,  -41.57896D,  72.21265D,  -27.43367D,  70.05180D,  -25.37802D,  71.11107D], $
 [ -41.57896D,  72.21265D,  -42.80684D,  71.01292D,  -29.28523D,  68.97046D,  -27.43367D,  70.05180D], $
 [ -42.80684D,  71.01292D,  -43.89681D,  69.80616D,  -30.95923D,  67.87024D,  -29.28523D,  68.97046D], $
 [ -44.33547D,  69.85006D,  -45.28816D,  68.63514D,  -32.83500D,  66.82492D,  -31.32800D,  67.94490D], $
 [ -45.28816D,  68.63514D,  -46.14581D,  67.41546D,  -34.20697D,  65.69120D,  -32.83500D,  66.82492D], $
 [ -46.14581D,  67.41546D,  -46.92271D,  66.19170D,  -35.46057D,  64.54554D,  -34.20697D,  65.69120D], $
 [ -47.30234D,  66.22866D,  -47.99347D,  64.99980D,  -36.93429D,  63.45083D,  -35.79576D,  64.60965D], $
 [ -47.99347D,  64.99980D,  -48.62642D,  63.76801D,  -37.98144D,  62.28306D,  -36.93429D,  63.45083D], $
 [ -48.97509D,  63.80116D,  -49.54362D,  62.56554D,  -39.25286D,  61.16321D,  -38.29658D,  62.34120D], $  ;  40
 [ -49.54362D,  62.56554D,  -50.06924D,  61.32772D,  -40.13779D,  59.97840D,  -39.25286D,  61.16321D], $
 [ -50.39178D,  61.35775D,  -50.86794D,  60.11698D,  -41.24696D,  58.83855D,  -40.43459D,  60.03142D], $
 [ -50.86794D,  60.11698D,  -51.31180D,  58.87451D,  -42.00294D,  57.64037D,  -41.24696D,  58.83855D], $
 [ -51.61212D,  58.90194D,  -52.01727D,  57.65713D,  -42.98060D,  56.48433D,  -42.28320D,  57.68896D], $
 [ -52.30792D,  57.68310D,  -52.67898D,  56.43623D,  -43.89834D,  55.32024D,  -43.25359D,  56.53063D], $
 [ -52.67898D,  56.43623D,  -53.02858D,  55.18816D,  -44.50283D,  54.10617D,  -43.89834D,  55.32024D], $
 [ -53.30153D,  55.21216D,  -53.62383D,  53.96240D,  -45.32304D,  52.93010D,  -44.76174D,  54.14888D], $
 [ -53.62383D,  53.96240D,  -53.92934D,  52.71164D,  -45.85162D,  51.70833D,  -45.32304D,  52.93010D], $
 [ -54.18699D,  52.73397D,  -54.47032D,  51.48179D,  -46.59077D,  50.52223D,  -46.09796D,  51.74790D], $
 [ -54.72119D,  51.50312D,  -54.98469D,  50.24966D,  -47.29185D,  49.33091D,  -46.83156D,  50.56012D], $  ;  50
 [ -54.98469D,  50.24966D,  -55.23650D,  48.99542D,  -47.72789D,  48.09951D,  -47.29185D,  49.33091D], $
 [ -55.47492D,  49.01545D,  -55.71036D,  47.76011D,  -48.36678D,  46.90041D,  -47.95805D,  48.13480D], $
 [ -55.94322D,  47.77932D,  -56.16391D,  46.52298D,  -48.97601D,  45.69718D,  -48.59223D,  46.93429D], $
 [ -56.39155D,  46.54144D,  -56.59893D,  45.28417D,  -49.55790D,  44.49012D,  -49.19699D,  45.72973D], $
 [ -56.59893D,  45.28417D,  -56.79900D,  44.02629D,  -49.90212D,  43.24903D,  -49.55790D,  44.49012D], $
 [ -57.01702D,  44.04382D,  -57.20592D,  42.78512D,  -50.43932D,  42.03629D,  -50.11460D,  43.27953D], $
 [ -57.41962D,  42.80205D,  -57.59838D,  41.54259D,  -50.95481D,  40.82044D,  -50.64801D,  42.06565D], $
 [ -57.80802D,  41.55895D,  -57.97759D,  40.29879D,  -51.45016D,  39.60169D,  -51.15990D,  40.84873D], $
 [ -58.18340D,  40.31462D,  -58.34461D,  39.05380D,  -51.92686D,  38.38025D,  -51.65185D,  39.62897D], $
 [ -58.34461D,  39.05380D,  -58.50170D,  37.79249D,  -52.19099D,  37.13056D,  -51.92686D,  38.38025D], $  ;  60
 [ -58.70044D,  37.80772D,  -58.85042D,  36.54581D,  -52.63716D,  35.90512D,  -52.38625D,  37.15628D], $
 [ -59.04597D,  36.56061D,  -59.18947D,  35.29814D,  -53.06820D,  34.67743D,  -52.82953D,  35.92997D], $
 [ -59.38202D,  35.31255D,  -59.51963D,  34.04955D,  -53.48517D,  33.44764D,  -53.25785D,  34.70145D], $
 [ -59.70935D,  34.06360D,  -59.84159D,  32.80011D,  -53.88901D,  32.21587D,  -53.67224D,  33.47086D], $
 [ -60.02864D,  32.81382D,  -60.15600D,  31.54986D,  -54.28062D,  30.98224D,  -54.07365D,  32.23834D], $
 [ -60.34054D,  31.56327D,  -60.46346D,  30.29887D,  -54.66082D,  29.74687D,  -54.46296D,  31.00400D], $
 [ -60.64564D,  30.31199D,  -60.76453D,  29.04718D,  -55.03036D,  28.50987D,  -54.84099D,  29.76795D], $
 [ -60.94449D,  29.06005D,  -61.05972D,  27.79485D,  -55.38996D,  27.27132D,  -55.20849D,  28.53030D], $
 [ -61.23759D,  27.80748D,  -61.34951D,  26.54192D,  -55.74025D,  26.03133D,  -55.56615D,  27.29114D], $
 [ -61.52543D,  26.55434D,  -61.63436D,  25.28843D,  -56.08187D,  24.78996D,  -55.91464D,  26.05056D], $  ;  70
 [ -61.80844D,  25.30066D,  -61.91468D,  24.03442D,  -56.41537D,  23.54732D,  -56.25455D,  24.80865D], $
 [ -62.08705D,  24.04649D,  -62.19088D,  22.77995D,  -56.74129D,  22.30347D,  -56.58645D,  23.56547D], $
 [ -62.36164D,  22.79186D,  -62.46333D,  21.52504D,  -57.06013D,  21.05848D,  -56.91088D,  22.32112D], $
 [ -62.63259D,  21.53682D,  -62.73237D,  20.26974D,  -57.37236D,  19.81242D,  -57.22833D,  21.07566D], $
 [ -62.90024D,  20.28141D,  -62.99835D,  19.01408D,  -57.67842D,  18.56536D,  -57.53926D,  19.82915D], $
 [ -63.16493D,  19.02566D,  -63.26159D,  17.75809D,  -57.97873D,  17.31736D,  -57.84411D,  18.58165D], $
 [ -63.42697D,  17.76960D,  -63.52239D,  16.50183D,  -58.27367D,  16.06847D,  -58.14330D,  17.33324D], $
 [ -63.68666D,  16.51327D,  -63.78103D,  15.24531D,  -58.56363D,  14.81875D,  -58.43722D,  16.08396D], $
 [ -63.94430D,  15.25671D,  -64.03781D,  13.98858D,  -58.84895D,  13.56825D,  -58.72623D,  14.83387D], $
 [ -64.20015D,  13.99995D,  -64.29299D,  12.73167D,  -59.12997D,  12.31704D,  -59.01069D,  13.58302D], $  ;  80
 [ -64.45450D,  12.74303D,  -64.54683D,  11.47462D,  -59.40701D,  11.06514D,  -59.29094D,  12.33147D], $
 [ -64.70759D,  11.48598D,  -64.79959D,  10.21745D,  -59.68039D,   9.81262D,  -59.56729D,  11.07927D], $
 [ -64.95968D,  10.22883D,  -65.05152D,   8.96020D,  -59.95038D,   8.55952D,  -59.84005D,   9.82645D], $
 [ -65.21101D,   8.97161D,  -65.30285D,   7.70292D,  -60.21729D,   7.30588D,  -60.10951D,   8.57306D], $
 [ -65.46184D,   7.71437D,  -65.55383D,   6.44562D,  -60.48137D,   6.05174D,  -60.37595D,   7.31916D], $
 [ -65.87093D,   6.46860D,  -65.96289D,   5.19994D,  -60.90086D,   4.80996D,  -60.79795D,   6.07777D], $
 [ -66.12108D,   5.21149D,  -66.21359D,   3.94281D,  -61.15985D,   3.55475D,  -61.05886D,   4.82273D], $
 [ -66.37149D,   3.95446D,  -66.46471D,   2.68580D,  -61.41687D,   2.29918D,  -61.31761D,   3.56731D], $
 [ -66.62241D,   2.69756D,  -66.71648D,   1.42893D,  -61.67215D,   1.04329D,  -61.57446D,   2.31154D], $
 [ -66.87405D,   1.44082D,  -66.96914D,   0.17224D,  -61.92594D,  -0.21288D,  -61.82966D,   1.05548D], $  ;  90
 [ -67.12665D,   0.18427D,  -67.22292D,  -1.08424D,  -62.17847D,  -1.46929D,  -62.08343D,  -0.20086D], $
 [ -67.38044D,  -1.07206D,  -67.47804D,  -2.34046D,  -62.42997D,  -2.72591D,  -62.33602D,  -1.45742D], $
 [ -67.63566D,  -2.32811D,  -67.73476D,  -3.59640D,  -62.68068D,  -3.98270D,  -62.58765D,  -2.71417D], $
 [ -67.89254D,  -3.58386D,  -67.99332D,  -4.85200D,  -62.93082D,  -5.23962D,  -62.83857D,  -3.97108D], $
 [ -68.15134D,  -4.83927D,  -68.25395D,  -6.10724D,  -63.18063D,  -6.49665D,  -63.08900D,  -5.22811D], $
 [ -68.41229D,  -6.09429D,  -68.51693D,  -7.36207D,  -63.43033D,  -7.75374D,  -63.33916D,  -6.48523D], $
 [ -68.83435D,  -7.33566D,  -68.94169D,  -8.60303D,  -63.83964D,  -8.99959D,  -63.74828D,  -7.73100D], $
 [ -69.10085D,  -8.58954D,  -69.21068D,  -9.85666D,  -64.09042D, -10.25676D,  -63.99914D,  -8.98825D], $
 [ -69.37038D,  -9.84291D,  -69.48290D, -11.10975D,  -64.34188D, -11.51389D,  -64.25051D, -10.24546D], $
 [ -69.64322D, -11.09570D,  -69.75863D, -12.36224D,  -64.59427D, -12.77093D,  -64.50264D, -11.50261D], $  ; 100
 [ -69.91965D, -12.34788D,  -70.03820D, -13.61409D,  -64.84783D, -14.02785D,  -64.75579D, -12.75966D], $
 [ -70.19999D, -13.59941D,  -70.32190D, -14.86525D,  -65.10283D, -15.28463D,  -65.01019D, -14.01658D], $
 [ -70.48456D, -14.85023D,  -70.61008D, -16.11567D,  -65.35953D, -16.54121D,  -65.26611D, -15.27334D], $
 [ -70.77368D, -16.10029D,  -70.90309D, -17.36529D,  -65.61820D, -17.79758D,  -65.52382D, -16.52989D], $
 [ -71.06772D, -17.34954D,  -71.20129D, -18.61408D,  -65.87914D, -19.05370D,  -65.78360D, -17.78621D], $
 [ -71.36704D, -18.59792D,  -71.50507D, -19.86196D,  -66.14263D, -20.30953D,  -66.04573D, -19.04226D], $
 [ -71.67203D, -19.84538D,  -71.81484D, -21.10888D,  -66.40900D, -21.56504D,  -66.31052D, -20.29801D], $
 [ -71.98311D, -21.09185D,  -72.13105D, -22.35477D,  -66.67857D, -22.82018D,  -66.57829D, -21.55341D], $
 [ -72.30072D, -22.33728D,  -72.45415D, -23.59958D,  -66.95168D, -24.07493D,  -66.84936D, -22.80844D], $
 [ -72.62532D, -23.58160D,  -72.78464D, -24.84323D,  -67.22871D, -25.32925D,  -67.12408D, -24.06305D], $  ; 110
 [ -72.95741D, -24.82473D,  -73.12304D, -26.08564D,  -67.51004D, -26.58309D,  -67.40283D, -25.31721D], $
 [ -73.29752D, -26.06660D,  -73.46991D, -27.32675D,  -67.79608D, -27.83641D,  -67.68600D, -26.57087D], $
 [ -73.64621D, -27.30713D,  -73.82586D, -28.56646D,  -68.08727D, -29.08918D,  -67.97400D, -27.82400D], $
 [ -74.00410D, -28.54624D,  -74.19154D, -29.80469D,  -68.38408D, -30.34135D,  -68.26728D, -29.07654D], $
 [ -74.37184D, -29.78384D,  -74.56764D, -31.04135D,  -68.68702D, -31.59287D,  -68.56632D, -30.32847D], $
 [ -74.75012D, -31.01983D,  -74.95492D, -32.27634D,  -68.99662D, -32.84369D,  -68.87162D, -31.57972D], $
 [ -75.13970D, -32.25411D,  -75.35417D, -33.50953D,  -69.31348D, -34.09377D,  -69.18374D, -32.83025D], $
 [ -75.54140D, -33.48657D,  -75.76628D, -34.74083D,  -69.63822D, -35.34304D,  -69.50327D, -34.08000D], $
 [ -75.95610D, -34.71709D,  -76.19220D, -35.97011D,  -69.97152D, -36.59146D,  -69.83085D, -35.32892D], $
 [ -76.38475D, -35.94554D,  -76.63295D, -37.19721D,  -70.31414D, -37.83895D,  -70.16717D, -36.57695D], $  ; 120
 [ -76.63295D, -37.19721D,  -76.89098D, -38.44802D,  -70.46458D, -39.10046D,  -70.31414D, -37.83895D], $
 [ -77.08965D, -38.42201D,  -77.36162D, -39.67130D,  -70.82471D, -40.34637D,  -70.66688D, -39.08544D], $
 [ -77.56353D, -39.64434D,  -77.85058D, -40.89197D,  -71.19661D, -41.59116D,  -71.03064D, -40.33087D], $
 [ -78.05591D, -40.86401D,  -78.35931D, -42.10986D,  -71.58131D, -42.83477D,  -71.40637D, -41.57516D], $
 [ -78.56826D, -42.08084D,  -78.88941D, -43.32476D,  -71.97998D, -44.07710D,  -71.79515D, -42.81822D], $
 [ -79.10216D, -43.29462D,  -79.44260D, -44.53644D,  -72.39391D, -45.31804D,  -72.19817D, -44.05994D], $
 [ -79.44260D, -44.53644D,  -79.79952D, -45.77680D,  -72.59668D, -46.57553D,  -72.39391D, -45.31804D], $
 [ -80.02082D, -45.74464D,  -80.40037D, -46.98257D,  -73.04029D, -47.81408D,  -72.82451D, -46.55748D], $
 [ -80.62618D, -46.94909D,  -81.03046D, -48.18435D,  -73.50356D, -49.05094D,  -73.27335D, -47.79531D], $
 [ -81.26101D, -48.14947D,  -81.69235D, -49.38180D,  -73.98845D, -50.28594D,  -73.74221D, -49.03137D], $  ; 130
 [ -81.69235D, -49.38180D,  -82.14765D, -50.61200D,  -74.24597D, -51.53976D,  -73.98845D, -50.28594D], $
 [ -82.38891D, -50.57455D,  -82.87646D, -51.80129D,  -74.77407D, -52.77152D,  -74.49712D, -51.51893D], $
 [ -83.12332D, -51.76218D,  -83.64636D, -52.98509D,  -75.33071D, -54.00093D,  -75.03203D, -52.74970D], $
 [ -83.64636D, -52.98509D,  -84.20178D, -54.20509D,  -75.64566D, -55.25119D,  -75.33071D, -54.00093D], $
 [ -84.46131D, -54.16287D,  -85.05961D, -55.37825D,  -76.26074D, -56.47639D,  -75.91900D, -55.22775D], $
 [ -85.05961D, -55.37825D,  -85.69778D, -56.59003D,  -76.62330D, -57.72388D,  -76.26074D, -56.47639D], $
 [ -85.97154D, -56.54427D,  -86.66203D, -57.75041D,  -77.31044D, -58.94404D,  -76.91443D, -57.69853D], $
 [ -86.94314D, -57.70238D,  -87.69189D, -58.90219D,  -78.04535D, -60.16052D,  -77.61132D, -58.91726D], $
 [ -87.69189D, -58.90219D,  -88.49660D, -60.09681D,  -78.51084D, -61.40216D,  -78.04535D, -60.16052D], $
 [ -88.79445D, -60.04439D,  -89.67140D, -61.23111D,  -79.34820D, -62.61165D,  -78.83409D, -61.37284D], $  ; 140
 [ -89.67140D, -61.23111D,  -90.61925D, -62.41114D,  -79.90418D, -63.84836D,  -79.34820D, -62.61165D], $
 [ -90.93570D, -62.35363D,  -91.97415D, -63.52364D,  -80.87314D, -65.04902D,  -80.25377D, -63.81598D], $
 [ -91.97415D, -63.52364D,  -93.10357D, -64.68489D,  -81.54940D, -66.27926D,  -80.87314D, -65.04902D], $
 [ -93.10357D, -64.68489D,  -94.33619D, -65.83613D,  -82.29169D, -67.50629D,  -81.54940D, -66.27926D], $
 [ -94.68476D, -65.76974D,  -96.04719D, -66.90632D,  -83.53093D, -68.68993D,  -82.69102D, -67.46849D], $
 [ -96.04719D, -66.90632D,  -97.54448D, -68.02940D,  -84.46408D, -69.90682D,  -83.53093D, -68.68993D], $
 [ -97.91569D, -67.95546D,  -99.58032D, -69.05906D,  -85.97347D, -71.07297D,  -84.90471D, -69.86392D], $
 [ -99.58032D, -69.05906D, -101.42280D, -70.14408D,  -87.17793D, -72.27528D,  -85.97347D, -71.07297D], $
 [-101.42280D, -70.14408D, -103.47006D, -71.20734D,  -88.54666D, -73.46944D,  -87.17793D, -72.27528D], $
 [-103.87582D, -71.11989D, -106.17058D, -72.15249D,  -90.67082D, -74.59713D,  -89.06739D, -73.41674D], $  ; 150
 [-106.17058D, -72.15249D, -108.73636D, -73.15429D,  -92.52703D, -75.76449D,  -90.67082D, -74.59713D], $
 [-108.73636D, -73.15429D, -111.61373D, -74.11949D,  -94.69959D, -76.91541D,  -92.52703D, -75.76449D], $
 [-111.61373D, -74.11949D, -114.84774D, -75.04105D,  -97.27323D, -78.04520D,  -94.69959D, -76.91541D], $
 [-114.84774D, -75.04105D, -118.48664D, -75.91043D, -100.36185D, -79.14731D,  -97.27323D, -78.04520D], $
 [-118.91429D, -75.79263D, -122.99481D, -76.59258D, -104.87911D, -80.12329D, -101.07713D, -79.06658D], $
 [-122.99481D, -76.59258D, -127.56077D, -77.31800D, -109.54971D, -81.12814D, -104.87911D, -80.12329D], $
 [-127.56077D, -77.31800D, -132.63868D, -77.95545D, -115.34659D, -82.06146D, -109.54971D, -81.12814D], $
 [-132.63868D, -77.95545D, -138.22996D, -78.49033D, -122.57406D, -82.89508D, -115.34659D, -82.06146D], $
 [-138.22996D, -78.49033D, -144.29845D, -78.90781D, -131.52044D, -83.59002D, -122.57406D, -82.89508D], $
 [-144.29845D, -78.90781D, -150.76044D, -79.19430D, -142.29359D, -84.09716D, -131.52044D, -83.59002D], $  ; 160
 [-150.76044D, -79.19430D, -157.48294D, -79.33923D, -154.56098D, -84.36556D, -142.29359D, -84.09716D], $
 [-157.48294D, -79.33923D, -164.29501D, -79.33684D, -167.40857D, -84.36106D, -154.56098D, -84.36556D], $
 [-164.29501D, -79.33684D, -171.01163D, -79.18722D, -179.63915D, -84.08428D, -167.40857D, -84.36106D], $
 [-171.01163D, -79.18722D, -177.46276D, -78.89633D,  169.64443D, -83.57027D, -179.63915D, -84.08428D], $
 [-177.46276D, -78.89633D,  176.48310D, -78.47485D,  160.75673D, -82.87014D,  169.64443D, -83.57027D], $
 [ 176.48310D, -78.47485D,  170.90803D, -77.93643D,  153.58024D, -82.03276D,  160.75673D, -82.87014D], $
 [ 170.90803D, -77.93643D,  165.84681D, -77.29592D,  147.82424D, -81.09674D,  153.58024D, -82.03276D], $
 [ 165.84681D, -77.29592D,  161.29701D, -76.56789D,  143.18534D, -80.08996D,  147.82424D, -81.09674D], $
 [ 161.29701D, -76.56789D,  157.23150D, -75.76575D,  139.40772D, -79.03183D,  143.18534D, -80.08996D], $
 [ 157.23150D, -75.76575D,  153.60960D, -74.90127D,  136.29261D, -77.93582D,  139.40772D, -79.03183D], $  ; 170
 [ 153.60960D, -74.90127D,  150.38517D, -73.98452D,  133.69038D, -76.81131D,  136.29261D, -77.93582D], $
 [ 150.38517D, -73.98452D,  147.51189D, -73.02393D,  131.48935D, -75.66496D,  133.69038D, -76.81131D], $
 [ 147.09012D, -73.12254D,  144.53368D, -72.11967D,  129.05657D, -74.55928D,  130.90374D, -75.72711D], $
 [ 144.53368D, -72.11967D,  142.24703D, -71.08616D,  127.46041D, -73.37852D,  129.05657D, -74.55928D], $
 [ 142.24703D, -71.08616D,  140.19469D, -70.02673D,  126.06737D, -72.18734D,  127.46041D, -73.37852D], $
 [ 139.80146D, -70.10944D,  137.96510D, -69.02377D,  124.37811D, -71.03411D,  125.57779D, -72.23665D], $
 [ 137.96510D, -69.02377D,  136.30580D, -67.91962D,  123.31333D, -69.82485D,  124.37811D, -71.03411D], $
 [ 136.30580D, -67.91962D,  134.80071D, -66.79958D,  122.36116D, -68.60998D,  123.31333D, -69.82485D], $
 [ 134.44235D, -66.86989D,  133.08398D, -65.73291D,  121.10640D, -67.42906D,  121.94356D, -68.65064D], $
 [ 133.08398D, -65.73291D,  131.84320D, -64.58426D,  120.34799D, -66.20361D,  121.10640D, -67.42906D]]    ; 180

;---------------------------------------------------------------------------
; Add the offset to the 4 corner longitude coordinates in each block.
;---------------------------------------------------------------------------

LatLonCoords = LLCornerCoords[*, BlkBeg-1 : BlkEnd-1]

FOR ipt=0,7,2 DO LatLonCoords[ipt, *] += !KON.Instr.DEG_OFFSET * (Path - 1)

ndxs = WHERE(LatLonCoords LT WrapLon, numndxs)
IF (numndxs) THEN LatLonCoords[ndxs] += 360.

ndxs = 0

END  ;  GetPathLatLonCoords

;***************************************************************************
PRO Get_OrbitTime_OrbitNumber, JulianTime, OnlyValidBlock, OrbitNumber, $
                               BlockNumber, Status
;***************************************************************************
; David Nelson - November 2012.
; Function computes either the Julian time given an orbit and block number
; or the orbit and block number given a Julian time. It is loosely based on
; Brian's MISR toolkit code but is extensively rewritten.
; The list of Julian dates represents ASCENDING node times (equator crossing
; time on dark side) for every 1000th orbit. The time at a specific block
; is calculated by adding (block number + ASCNODE_TO_BLK1) * per-block
; transit time to the ascending node time.
; The MISR_ORBIT_REF table provides adjustments for drift in the orbit.
; First find an approximate orbit to the nearest thousand, then use it to
; find a more accurate entry in the table.
; These values were computed from the latest file in ./anc/ORBITPRED
; directory. Entries without a date or time are simply extrapolated using
; the mean delta time from a combination of the computed entries and
; DAYS_PER_ORBIT. These should be replaced as those orbits are acquired.
; When computing orbit and block, seconds must be included in the Julian
; time or the block number may be in error by as much as 3 blocks.
; Because of variations in the orbit, the block prediction returned below is
; occasionally off by 1 block. Time predictions are accurate to +/- 15 sec.
;---------------------------------------------------------------------------

COMPILE_OPT idl2, LOGICAL_PREDICATE, STRICTARRSUBS

Status = -1

; Values below are hardwired to reduce compute time. The MISR_ORBIT_REF table
; has been moved to global memory (!KON) to reduce overhead when thousands of
; calculations are made.

FIRST_ORBIT     = 1L         ; ndx of first reliable orbit to use in calculations
LAST_ORBIT      = 80L        ; ndx of last current orbit for orbit duration calculation
ORBIT_INCR      = 1000L      ; number of orbits between entries in table
BLKS_PER_ORBIT  = 283.669D   ; 2.0 * !DPI * EarthPolarRadius / (512.0 * 0.275D)
ASCNODE_TO_BLK1 = 52.152609D ; (0.5 - 90.0 / BLKS_PER_ORBIT) * BLKS_PER_ORBIT
MIN_BLOCK       = -53        ; FLOOR(90. - BLKS_PER_ORBIT / 2.0)
MAX_BLOCK       = 233        ; CEIL (90. + BLKS_PER_ORBIT / 2.0)
NUM_REFS        = N_ELEMENTS(!KON.MISR_ORBIT_REF)

;---------------------------------------------------------------------------
; Compute the approximate number of days it takes to complete a single orbit.
; Make it the average of (days per repeat cycle / number of orbits in repeat
; cycle) and (mean difference from orbit ref table).
;---------------------------------------------------------------------------

del_time  = (!KON.MISR_ORBIT_REF[LAST_ORBIT] - !KON.MISR_ORBIT_REF[FIRST_ORBIT])
del_orbit = (LAST_ORBIT - FIRST_ORBIT) * ORBIT_INCR
DAYS_PER_ORBIT = (16.0D / 233.0D + del_time / DOUBLE(del_orbit)) / 2.0D
DAYS_PER_BLOCK = DAYS_PER_ORBIT / BLKS_PER_ORBIT

;---------------------------------------------------------------------------
; Compute orbit number and block number from orbit time.
;---------------------------------------------------------------------------

IF (OrbitNumber LT 0 AND BlockNumber LT 0 AND JulianTime GT 0.0) THEN BEGIN
   
   ;------------------------------------------------------------------------
   ; Compute orbit number by finding nearest orbit in orbit reference table
   ; and then interpolating.
   ;------------------------------------------------------------------------

   ref_num = LONG(((JulianTime - !KON.MISR_ORBIT_REF[FIRST_ORBIT]) / $
                   DAYS_PER_ORBIT + FIRST_ORBIT * ORBIT_INCR) / ORBIT_INCR)

   orbit_ndx = (ref_num LT NUM_REFS) ? ref_num : (NUM_REFS - 1)

   xtra_orbits = (JulianTime - !KON.MISR_ORBIT_REF[orbit_ndx]) / DAYS_PER_ORBIT
   OrbitNumber = FLOOR(xtra_orbits) + orbit_ndx * ORBIT_INCR

   ;------------------------------------------------------------------------
   ; Get block number from the fractional part of orbit relative to the
   ; ascending node. Block number isn't reliable without seconds!
   ;------------------------------------------------------------------------

   frac_orbits = xtra_orbits - FLOOR(xtra_orbits)
   BlockNumber = ROUND(frac_orbits * BLKS_PER_ORBIT - ASCNODE_TO_BLK1 - 0.5)

   ;------------------------------------------------------------------------
   ; Return the block number relative to orbit start at ascending node. The
   ; range is from -52 to 233. Status is OK unless the block number falls
   ; outside this range. This shouldn't happen.
   ;------------------------------------------------------------------------

   IF ((~OnlyValidBlock AND BlockNumber GE MIN_BLOCK $
                        AND BlockNumber LE MAX_BLOCK) OR $
       ( OnlyValidBlock AND BlockNumber GE 1 $
                        AND BlockNumber LE !KON.Instr.NUM_BLOCKS)) THEN BEGIN
         Status = 0
   ENDIF ELSE BEGIN
      BlockNumber = FIX(!KON.MISC.BADVALUE_REAL)
   ENDELSE

ENDIF

;---------------------------------------------------------------------------
; Compute orbit time from orbit number and block number.
;---------------------------------------------------------------------------

IF (OrbitNumber GT 0 AND BlockNumber GT 0 AND JulianTime LT 0.0) THEN BEGIN
   
   ;------------------------------------------------------------------------
   ; Compute Julian time using the ascending node time at orbit start and
   ; interpolating between entries.
   ;------------------------------------------------------------------------

   ref_num = LONG(OrbitNumber / ORBIT_INCR)

   orbit_ndx = (ref_num LE NUM_REFS) ? ref_num : NUM_REFS - 1

   JulianTime = !KON.MISR_ORBIT_REF[orbit_ndx] + DAYS_PER_ORBIT * $
                (OrbitNumber - orbit_ndx * ORBIT_INCR)

   ;------------------------------------------------------------------------
   ; Add time for block number relative to ascending node of orbit.
   ;------------------------------------------------------------------------

   JulianTime += (BlockNumber + ASCNODE_TO_BLK1 + 0.5) * DAYS_PER_BLOCK

   Status = 0
ENDIF

END  ;  Get_OrbitTime_OrbitNumber

;****************************************************************************
FUNCTION IsPtInPolygon, TestPt, PolyPts, NumPts
;****************************************************************************
; This function determines whether a point is contained within the area 
; circumscribed by a polygon. Each line segment of the polygon is  compared
; with an imaginary, horizontal half-line extending from the  test point to
; positive infinity. If an even number of line segments intersects the half-
; line, then the test point is outside the polygon. If an odd number crosses,
; then the test point is inside.
;
; passed params:  TestPt  - [2,1] array w/ point's X and Y coords
;                 PolyPts - [2,NumPts] array w/ polygon's X/Y coords
;                 NumPts  - number of points in Polygon arrays
;
; return values:  0 = point is inside polygon
;                 1 = point is outside polygon
;                 2 = point is entirely above polygon 
;                 3 = point is entirely below polygon        
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Using the minimum and maximum of the x and y polygon coordinates, try
   ; to eliminate test point here.
   ;------------------------------------------------------------------------

   IF (TestPt[1,0] LT MIN(PolyPts[1,*])) THEN RETURN, 3
   IF (TestPt[1,0] GT MAX(PolyPts[1,*])) THEN RETURN, 2

   ;------------------------------------------------------------------------
   ; Loop over all the line segments defining the polygon. Wrap around back
   ; to the first point so all line segments are tested.
   ;------------------------------------------------------------------------

   num_intersects = 0          

   FOR ipt = 0, NumPts-1  DO BEGIN

      ipt1 = (ipt+1) MOD 4
      y1   = PolyPts[1,ipt]
      y2   = PolyPts[1,ipt1]
      ymin = y1 < y2
      ymax = y1 > y2

      ;---------------------------------------------------------------------
      ; Test if this segment shares a Y-coordinate with horizontal line.
      ;---------------------------------------------------------------------

      IF (ymin LE TestPt[1,0] AND ymax GE TestPt[1,0]) THEN BEGIN

         x1   = PolyPts[0,ipt]
         x2   = PolyPts[0,ipt1]
         xmin = x1 < x2
         xmax = x1 > x2

         ;------------------------------------------------------------------
         ; Test the case where the polygon's line segment is horizontal to
         ; avoid a divide by zero in the general case.
         ;------------------------------------------------------------------

         IF (y1 EQ y2) THEN BEGIN

            ;---------------------------------------------------------------
            ; If the Y coordinate coincides with the horizontal test line,
            ; the line segment is considered to have crossed it if ONLY one
            ; endpoint is to the right of the test point.
            ;---------------------------------------------------------------

            IF (xmax GE TestPt[0,0] AND xmin LT TestPt[0,0]) THEN $
               num_intersects = num_intersects + 1

         ;------------------------------------------------------------------
         ; Test the general case here.
         ;------------------------------------------------------------------

         ENDIF ELSE BEGIN

            ;---------------------------------------------------------------
            ; Find the polygon line segment's X coordinate at the Y coord of
            ; the testpoint.
            ;---------------------------------------------------------------

            xtest = (x2 - x1) * (TestPt[1,0] - y1) / (y2 - y1) + x1
                         
            ;---------------------------------------------------------------
            ; Determine if the line segment's X coord is to the right of the
            ; test point, thus intersecting the imaginary horizontal line.
            ;--------------------------------------------------------------- 

            IF (xtest GE TestPt[0,0]) THEN $
               num_intersects = num_intersects + 1

         ENDELSE

      ENDIF

   ENDFOR

   ;------------------------------------------------------------------------
   ; If an even number of intersections were found, test point is external
   ; to polygon. Otherwise, test point is internal.
   ;------------------------------------------------------------------------
    
   IF (num_intersects MOD 2) EQ 0 THEN BEGIN
      RETURN, 1
   ENDIF ELSE BEGIN
      RETURN, 0
   ENDELSE

END  ;  IsPtInPolygon

;***************************************************************************
PRO DrawButton, xstart, ystart, xsize, ysize, btn_color, $
                text_color, text
;***************************************************************************
; This function draws a "button" to enable user to print the contents of the
; current window to a TIFF file.
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   xcoords = INTARR(5)
   ycoords = INTARR(5)

   ;------------------------------------------------------------------------
   ; Draw the button outline and fill it.
   ;------------------------------------------------------------------------

   xcoords[0] = xstart
   ycoords[0] = ystart
   xcoords[1] = xcoords[0] + xsize
   ycoords[1] = ycoords[0]
   xcoords[2] = xcoords[1]
   ycoords[2] = ycoords[0] - ysize
   xcoords[3] = xcoords[0]
   ycoords[3] = ycoords[2]
   xcoords[4] = xcoords[0]
   ycoords[4] = ycoords[0]

   POLYFILL, xcoords, ycoords, COLOR=btn_color, /DEVICE   
   PLOTS, xcoords, ycoords, /DEVICE, COLOR=text_color 

   ;------------------------------------------------------------------------
   ; Draw the text in the button.
   ;------------------------------------------------------------------------

   xboot = 5
   yboot = (!KON.Misc.MINX_PLATFORM EQ 2) ? 12 : 13

   XYOUTS, xstart+xboot, ystart-yboot, text, COLOR=text_color, $
           /DEVICE, CHARSIZE=1, ALIGNMENT=0.0
   RETURN

END  ;  DrawButton

;***************************************************************************
FUNCTION ImageToPngFile, window_id, batch_png_file, OrbitNum, $
                          xstart1, ystart1, xsize1, ysize1, $
                          xstart2, ystart2, xsize2, ysize2, $
                          xstart3, ystart3, xsize3, ysize3, latlon_str
;***************************************************************************
; This function saves an image to file in TIFF format.
;---------------------------------------------------------------------------

   COMPILE_OPT IDL2, LOGICAL_PREDICATE

   ;------------------------------------------------------------------------
   ; Get the name of the file in which to save the image.
   ;------------------------------------------------------------------------ 

   CreateOrbitDirFile, '', 'LocationMap_Orbit', '', 1, 'png', $
                       OrbitNum, 1, image_inpath, def_filename, Retval

   filename = DIALOG_PICKFILE(/WRITE, FILTER='*.png', PATH=image_inpath, $
                     FILE=def_filename, GET_PATH=temp_outpath, $
                     TITLE="Specify output PNG filename")

   IF (filename EQ '') THEN RETURN, 0
   slen = STRLEN(filename)
   ext = STRMID(filename, slen - 4, 4)
   IF (ext NE '.png') THEN filename = filename + '.png'

   npos = STRPOS(filename, !KON.Misc.Slash, /REVERSE_SEARCH) + 1
   tempdir = STRMID(filename, 0, npos)
   tempfile = STRMID(filename, npos)

   IF (~ FILE_TEST(tempdir, /DIRECTORY)) THEN BEGIN
      rtrn_val = MakeDirectory(tempdir)
      rtrn_val = ChmodCatchError(tempdir, '777'O)
   ENDIF

   ;------------------------------------------------------------------------
   ; Acquire the image and invert it.  Make the button as inconspicuous as
   ; possible before saving, and restore it after
   ;------------------------------------------------------------------------ 

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.5 : 1.5

   DrawButton, xstart1, ystart1, xsize1, ysize1, !KON.Colors.lt_blue, $
               !KON.Colors.lt_blue, 'Save image'

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.5 : 1.5

   DrawButton, xstart2, ystart2, xsize2, ysize2, !KON.Colors.lt_blue, $
               !KON.Colors.lt_blue, 'Continue'
   IF (latlon_str NE '') THEN $
      DrawButton, xstart3, ystart3, xsize3, ysize3, !KON.Colors.lt_blue, $
                  !KON.Colors.lt_blue, latlon_str

   current_wndw = !D.WINDOW
   SafeWSET, window_id, didit
   SafeWSHOW, window_id, 1, 0, didit
   saveimage = TVRD(/ORDER, TRUE=1)
   SafeWSET, current_wndw, didit
   DrawButton, xstart1, ystart1, xsize1, ysize1, !KON.Colors.red, $
               !KON.Colors.black, 'Save image'
   DrawButton, xstart2, ystart2, xsize2, ysize2, !KON.Colors.green, $
               !KON.Colors.black, 'Continue'
   IF (latlon_str NE '') THEN $
      DrawButton, xstart3, ystart3, xsize3, ysize3, !KON.Colors.yellow, $
                  !KON.Colors.black, latlon_str

   ;------------------------------------------------------------------------
   ; Write the image and palette to file.
   ;------------------------------------------------------------------------ 

   WRITE_PNG, filename, saveimage, /ORDER
   RETURN, 1

END  ;  ImageToPngFile

;***************************************************************************
PRO DisplayOrbitLocation, PathOrOrbit, PathNum, OrbitNum, BlockBeg, $
                          BlockEnd, status
;***************************************************************************
; This is the main procedure for the Show Orbit Location option. 
;---------------------------------------------------------------------------

COMPILE_OPT IDL2, LOGICAL_PREDICATE

   old_font = GetFontInfo(0)

   min_blks_for_17km = 1

   ;------------------------------------------------------------------------
   ; Get the path number from the orbit number is applicable.
   ;------------------------------------------------------------------------ 

   IF (PathOrOrbit EQ 2) THEN PathNum = PathFromOrbit(LONG(OrbitNum))

   ;------------------------------------------------------------------------
   ; Get screen size and size of title characters.
   ;------------------------------------------------------------------------ 

   xscr_max = !KON.Misc.ScreenX
   yscr_max = !KON.Misc.ScreenY

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'18', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5

   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   pwin = !D.WINDOW
   SafeWDELETE, pwin, didit

   ;------------------------------------------------------------------------
   ; Set the size of the non-usable x and y components of a window on the
   ; left and top so detail windows position can be held stable. This is
   ; a kludge and should be set to the size of the window border and title
   ; bar. Different OSs behave differently! Height of the title area inside
   ; window is measured as number of characters.
   ;------------------------------------------------------------------------
   
   num_char_y_margin = 3
   win1_title_size = FIX(ychr_max * num_char_y_margin)

   ;------------------------------------------------------------------------
   ; Create some arrays needed for data processing.
   ;------------------------------------------------------------------------

   lat = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)
   lon = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)

   lat2 = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)
   lon2 = FLTARR(!KON.Instr.NUM_BLOCKS+1, 4)

   ;------------------------------------------------------------------------
   ; Setup parameters to draw block outlines.
   ;------------------------------------------------------------------------

   nNumChoices     = 1
   nValidBlkBeg    = 0
   nValidBlkEnd    = (!KON.Instr.NUM_BLOCKS - 1)
   aryBlockNumbers = LINDGEN(!KON.Instr.NUM_BLOCKS) + 1
   coastline_resolution = 0
   nValidBlkBeg = BlockBeg - 1
   nValidBlkEnd = BlockEnd - 1
   Actual_First_Block = BlockBeg
   NumBlks = ABS(BlockEnd - BlockBeg) + 1
   IF (nValidBlkEnd - nValidBlkBeg LT 100) THEN coastline_resolution = 1
   IF (nValidBlkEnd - nValidBlkBeg LT  10) THEN coastline_resolution = 2

   ;------------------------------------------------------------------------
   ; Define vertical size of map in pixels.
   ;------------------------------------------------------------------------ 

   Yfactor = (yscr_max GT 1000) ? 0.9 : 0.95
   y_map_size = yscr_max * Yfactor - win1_title_size
   IF (NumBlks EQ min_blks_for_17km) THEN y_map_size /= 2

   ;------------------------------------------------------------------------
   ; Retrieve the AGP block corners data and get lat/long values for blocks
   ; for this path.
   ;------------------------------------------------------------------------

   GetPathLatLonCoords,  PathNum, 1, 180, -180.0, LatLonCoords
   GetSwathLatLonCoords, PathNum, 1, 180, -180.0, LatLonCoords2

   FOR iblk = nValidBlkBeg, nValidBlkEnd  DO BEGIN
      FOR ipt1=0,3 DO BEGIN
         ipt2 = ipt1 * 2
         lon[iblk,ipt1]  = LatLonCoords[ipt2,iblk]
         lat[iblk,ipt1]  = LatLonCoords[ipt2+1,iblk]
         lon2[iblk,ipt1] = LatLonCoords2[ipt2,iblk]
         lat2[iblk,ipt1] = LatLonCoords2[ipt2+1,iblk]
      ENDFOR
   ENDFOR

   LatLonCoords  = 0
   LatLonCoords2 = 0

   ;------------------------------------------------------------------------
   ; Create any arrays needed below before entering interactive loop.
   ;------------------------------------------------------------------------

   lonlatdat  = FLTARR(2,4)
   lonlatdat2 = FLTARR(2,4)

   ;------------------------------------------------------------------------
   ; Determine longitude plotting parameters - test for the case where the
   ; longitude crosses the dateline and Greenwich.
   ;------------------------------------------------------------------------

   blk1 = nValidBlkBeg
   blk2 = nValidBlkEnd
   center_block = (blk1 + blk2) / 2

   neglonmin = lon[center_block,0] < lon[center_block,2]
   neglonmax = lon[center_block,0] > lon[center_block,2]   

   IF ((neglonmin LT 0) AND (neglonmax GT 0) AND  $
        (ABS(lon[center_block,0]) GT 90.)) THEN BEGIN
      neglonmin = 360 + neglonmin
      lon_center = (neglonmin + neglonmax) / 2.
      IF lon_center GT 180 THEN lon_center = lon_center - 360
   ENDIF ELSE BEGIN
      lon_center = (neglonmin + neglonmax) / 2.
   ENDELSE

   ;------------------------------------------------------------------------
   ; Find the center latitude and compute the scale factor - scale of map
   ; as in 1 : 100,000 (map_scale = 100,000). Add some blocks as a fudge
   ; factor so the scale will never get too small. If zoomed in tight, make
   ; the map wider and the scale smaller. Running on Windows requires a
   ; different scale.
   ;------------------------------------------------------------------------

   blk_fudge = 10
   blk_diff = nValidBlkEnd - nValidBlkBeg + blk_fudge

   y_map_temp = y_map_size
   IF (NumBlks EQ min_blks_for_17km) THEN y_map_temp = y_map_size * 2

   x_map_size = y_map_temp / 2 + 25
   map_scale = 9.1E10 / y_map_temp * blk_diff / !KON.Instr.NUM_BLOCKS
   IF (!KON.Misc.MINX_PLATFORM EQ 2) THEN map_scale *= 0.8

   IF (blk_diff LT min_blks_for_17km + blk_fudge) THEN BEGIN
      x_map_size *= 2.0 
      map_scale  /= 2.0
   ENDIF

   lat_max = MAX(lat[blk1:blk2,*], MIN=lat_min, /NAN)
   lat_center = (lat_max + lat_min) / 2.

   IF (nValidBlkBeg LT nValidBlkEnd) THEN BEGIN
      IF (lat[blk1,0] LT lat[blk1+1,0])   THEN lat_center = lat_center + 7
      IF (lat[blk2,0] GT lat[blk2-1>1,0]) THEN lat_center = lat_center - 7
   ENDIF

   ;------------------------------------------------------------------------
   ; Create window and set proper size.
   ;------------------------------------------------------------------------

   bckgrnd_save = !P.BACKGROUND
   !P.BACKGROUND = !KON.Colors.blue1
   WINDOW, /FREE, XPOS=xscr_max-x_map_size-100, XSIZE=x_map_size, $
           YSIZE=y_map_size, RETAIN=2, TITLE='MINX V' + $
           !KON.Misc.MINX_VERSION_NUM + ' : MISR Orbit Location Tool'
   map_wndw = !D.WINDOW

   xview_max = !D.X_VSIZE 
   yview_max = !D.Y_VSIZE 

   ;------------------------------------------------------------------------
   ; Set map projection, scale etc. 
   ;------------------------------------------------------------------------

   MAP_SET, /LAMBERT, lat_center, lon_center, 0, /ISOTROPIC, /NOBORDER, $
            /HORIZON, COLOR=!KON.Colors.gray1, E_HORIZON={FILL:1, $
            COLOR:!KON.Colors.lt_blue}, SCALE=map_scale, XMARGIN=1.5, $
            YMARGIN=[1.0,num_char_y_margin]

   ;------------------------------------------------------------------------
   ; Set font for block numbers and fine detail.
   ;------------------------------------------------------------------------

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'10', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.2
   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   ;------------------------------------------------------------------------
   ; Determine location and frequency of lat/long annotations & lines.
   ;------------------------------------------------------------------------

   xygrid = FLTARR(2,3)
   llgrid = FLTARR(3,3)
   xygrid[0,0] = x_map_size - xchr_max * 4
   xygrid[0,1] = x_map_size - xchr_max * 4
   xygrid[0,2] = x_map_size - xchr_max * 4
   xygrid[1,0] = y_map_size / 5.
   xygrid[1,1] = y_map_size / 2.
   xygrid[1,2] = y_map_size / 5. * 4.
   llgrid = CONVERT_COORD(xygrid, /TO_DATA, /DEVICE)
   minll = MIN(llgrid[0,*], MAX=maxll)
   IF (minll * maxll LT 0.0 AND maxll GT 90.0) THEN BEGIN  ; dateline
      latlabel = maxll - 12.
   ENDIF ELSE BEGIN
      latlabel = minll - 3.
   ENDELSE
   lonlabel = (lat_min - 3.) > (-63)
   IF (blk_diff LT min_blks_for_17km + blk_fudge) THEN lonlabel = lat_min
   lldelta = 15.

   ;------------------------------------------------------------------------
   ; Adjust the lat-long grid to be appropriate to the scale.
   ;------------------------------------------------------------------------

   IF (blk_diff LT 120+blk_fudge) THEN lldelta = 10.
   IF (blk_diff LT  50+blk_fudge) THEN lldelta = 5.
   IF (blk_diff LT  20+blk_fudge) THEN lldelta = 2.
   IF (blk_diff LT   5+blk_fudge) THEN lldelta = 1.
   IF (blk_diff LT   2+blk_fudge) THEN lldelta = 0.5

   ;------------------------------------------------------------------------
   ; Draw continent fill, lat/long grid lines, coast and country lines.
   ;------------------------------------------------------------------------

   MAP_CONTINENTS, /FILL_CONTINENTS, COLOR=!KON.Colors.lt_green

   MAP_GRID, LATS=0, LATDEL=lldelta, LATLAB=latlabel, $
             LONS=0, LONDEL=lldelta, LONLAB=lonlabel, LABEL=1, $
             ORIENTATION=0, CHARSIZE=1, COLOR=!KON.Colors.red

   IF (coastline_resolution EQ 0) THEN BEGIN
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1    
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue2, MLINETHICK=1
   ENDIF
   IF (coastline_resolution EQ 1) THEN BEGIN
      MAP_CONTINENTS, /RIVERS,   COLOR=!KON.Colors.blue1, MLINETHICK=1
      MAP_CONTINENTS, /USA,      COLOR=!KON.Colors.brown, MLINETHICK=1
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue1 ,MLINETHICK=2
   ENDIF
   IF (coastline_resolution EQ 2) THEN BEGIN
      MAP_CONTINENTS, /RIVERS,   COLOR=!KON.Colors.blue1, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /USA,      COLOR=!KON.Colors.brown, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /COUNTRIES,COLOR=!KON.Colors.brown, MLINETHICK=1, /HIRES
      MAP_CONTINENTS, /COASTS,   COLOR=!KON.Colors.blue1 ,MLINETHICK=2, /HIRES
   ENDIF

   numdraw1 = 0
   numdraw2 = 0

   xysave0 = FLTARR(2,2)
   xysave1 = FLTARR(2)
   xysave1[0] = 0.0
   xysave1[1] = 0.0

   xysave02 = FLTARR(2,2)
   xysave12 = FLTARR(2)
   xysave12[0] = 0.0
   xysave12[1] = 0.0

   ;------------------------------------------------------------------------
   ; Loop over blocks.
   ;------------------------------------------------------------------------

   FOR iblk = nValidBlkBeg, nValidBlkEnd  DO BEGIN

      llblk = iblk + Actual_First_Block - nValidBlkBeg - 1

      ;---------------------------------------------------------------------
      ; set up current block's coordinates and convert to x/y coords
      ;---------------------------------------------------------------------

      lonlatdat[1,*] = lat[llblk,*]
      lonlatdat[0,*] = lon[llblk,*]

      xysave0[*,0] = xysave1
      xycoords = CONVERT_COORD(lonlatdat, /DATA, /TO_DEVICE)
      xysave1[0:1] = xycoords[0:1,1]

      IF (iblk EQ nValidBlkBeg) THEN BEGIN
         xysave0[0:1,0] = xycoords[0:1,0]
         xysave0[0:1,1] = xycoords[0:1,3]
      ENDIF ELSE BEGIN
         xysave0[0:1,1] = xycoords[0:1,0]
      ENDELSE

      ;---------------------------------------------------------------------
      ; set up the current swath's coordinates and convert to x/y coords
      ;---------------------------------------------------------------------

      lonlatdat2[1,0:1] = lat2[llblk,0:1]
      lonlatdat2[1,2:3] = lat2[llblk,2:3]
      lonlatdat2[0,0:1] = lon2[llblk,0:1]
      lonlatdat2[0,2:3] = lon2[llblk,2:3]

      xycoords2 = CONVERT_COORD(lonlatdat2, /DATA, /TO_DEVICE)

      ;---------------------------------------------------------------------
      ; draw line around block and around swath
      ;---------------------------------------------------------------------

      PLOTS, xycoords, /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
             THICK=1.0
      PLOTS, xysave0,  /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
             THICK=1.0

      PLOTS, xycoords2[0,0:1], xycoords2[1,0:1], /DEVICE, LINESTYLE=0, $
             COLOR=!KON.Colors.black, THICK=1.0
      PLOTS, xycoords2[0,2:3], xycoords2[1,2:3], /DEVICE, LINESTYLE=0, $
             COLOR=!KON.Colors.black, THICK=1.0

      ;---------------------------------------------------------------------
      ; If zoomed in tight, draw region boxes (17.6 km) inside blocks; there
      ; are 32x8 regions in a block; also label regions by region across and
      ; along numbers (1-based).
      ;---------------------------------------------------------------------

      IF (blk_diff LT min_blks_for_17km + blk_fudge) THEN BEGIN
         xdel = ((xycoords[0,3] - xycoords[0,0] + $
                  xycoords[0,2] - xycoords[0,1]) / 2.0) / 32.0
         ydel = ((xycoords[1,3] - xycoords[1,0] + $
                  xycoords[1,2] - xycoords[1,1]) / 2.0) / 32.0

         FOR ii=1,31 DO BEGIN
            xx = [xycoords[0,0]+ii*xdel,xycoords[0,1]+ii*xdel]
            yy = [xycoords[1,0]+ii*ydel,xycoords[1,1]+ii*ydel]
            PLOTS, xx,yy, /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
                   THICK=1.0
         ENDFOR

         xdel = ((xycoords[0,1] - xycoords[0,0] + $
                  xycoords[0,2] - xycoords[0,3]) / 2.0) / 8.0
         ydel = ((xycoords[1,1] - xycoords[1,0] + $
                  xycoords[1,2] - xycoords[1,3]) / 2.0) / 8.0

         FOR ii=1,7 DO BEGIN
            xx = [xycoords[0,0]+ii*xdel,xycoords[0,3]+ii*xdel]
            yy = [xycoords[1,0]+ii*ydel,xycoords[1,3]+ii*ydel]
            PLOTS, xx,yy, /DEVICE, LINESTYLE=0, COLOR=!KON.Colors.black, $
                   THICK=1.0
         ENDFOR

         xdel1 = ((xycoords[0,3] - xycoords[0,0] + $
                   xycoords[0,2] - xycoords[0,1]) / 2.0) / 32.0
         ydel1 = ((xycoords[1,3] - xycoords[1,0] + $
                   xycoords[1,2] - xycoords[1,1]) / 2.0) / 32.0
         xdel2 = ((xycoords[0,1] - xycoords[0,0] + $
                   xycoords[0,2] - xycoords[0,3]) / 2.0) / 8.0
         ydel2 = ((xycoords[1,1] - xycoords[1,0] + $
                   xycoords[1,2] - xycoords[1,3]) / 2.0) / 8.0
         half_rgn = xdel1 / 2.0
         FOR ii=0,31 DO BEGIN
            FOR jj=0,7 DO BEGIN
               rgn_nums = STRTRIM(STRING(ii+1),2) + ',' + $
                          STRTRIM(STRING(jj+1),2)
               XYOUTS, xycoords[0,0]+xdel1*ii+xdel2*jj+half_rgn, $
                       xycoords[1,0]+ydel1*ii+ydel2*jj-half_rgn-ychr_max/2,$
                       rgn_nums, /DEVICE, COLOR=!KON.Colors.black, $
                       CHARSIZE=0.8, ALIGNMENT=0.5
            ENDFOR
         ENDFOR
      ENDIF

      ;---------------------------------------------------------------------
      ; Annotate first block and every 5th block if displaying 130 - 180
      ; blocks, every 3rd block if displaying 80 - 129 blocks, every 2nd
      ; block if displaying 30 - 79 blocks, and every  block if displaying
      ; fewer than 30 blocks.
      ;---------------------------------------------------------------------

      iiblk = FIX(aryBlockNumbers[iblk] + Actual_First_Block - $
                  nValidBlkBeg - 1)
      IF (NumBlks GE 130) THEN BEGIN
         IF ((iiblk MOD 5) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE IF (NumBlks GE 80) THEN BEGIN
         IF ((iiblk MOD 3) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE IF (NumBlks GE 30) THEN BEGIN
         IF ((iiblk MOD 2) EQ 0 OR iiblk EQ Actual_First_Block) THEN BEGIN
            blknum_string = STRTRIM(STRING(iiblk),2)
            numdraw1 = numdraw1 + 1
            IF (numdraw1 EQ 1)  $
                THEN blk_string = 'Block # ' + blknum_string $
                ELSE blk_string = blknum_string
            XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                    ychr_max) / 2, blk_string, /DEVICE, $
                    COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
         ENDIF
      ENDIF ELSE BEGIN
         blknum_string = STRTRIM(STRING(iiblk),2)
         numdraw1 = numdraw1 + 1
         IF (numdraw1 EQ 1)  $
             THEN blk_string = 'Block # ' + blknum_string $
             ELSE blk_string = blknum_string
         XYOUTS, xycoords[0,1]-5, (xycoords[1,0] + xycoords[1,1] - $
                 ychr_max) / 2, blk_string, /DEVICE, $
                 COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=1.0
      ENDELSE
      
   ENDFOR

   ;------------------------------------------------------------------------
   ; Draw a button in upper right to let user save window image.
   ;------------------------------------------------------------------------
 
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5
   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   xstart_save = x_map_size - xchr_max * 16
   ystart_save = yview_max  - ychr_max * 5
   xsize_save  = xchr_max * 13
   ysize_save  = ychr_max * 1.3

   DrawButton, xstart_save, ystart_save, xsize_save, ysize_save, $
               !KON.Colors.red, !KON.Colors.black, 'Save image'

   ;------------------------------------------------------------------------
   ; Draw a button in lower left to let user close window.
   ;------------------------------------------------------------------------
 
   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'14', Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.5
   xchr_max = !D.X_CH_SIZE
   ychr_max = !D.Y_CH_SIZE

   xstart_exit = 30
   ystart_exit = ychr_max * 4
   xsize_exit  = xchr_max * 11
   ysize_exit  = ychr_max * 1.3

   DrawButton, xstart_exit, ystart_exit, xsize_exit, ysize_exit, $
               !KON.Colors.green, !KON.Colors.black, 'Continue'
   
   ;------------------------------------------------------------------------
   ; Set up the rectangle for reporting the lat/long coords of cursor.
   ;------------------------------------------------------------------------

   lonlat_rect = INTARR(2,5)
   lonlat_rect[0,0] = x_map_size - xchr_max * 30
   lonlat_rect[1,0] = ychr_max * 2.5
   lonlat_rect[0,1] = lonlat_rect[0,0]
   lonlat_rect[1,1] = lonlat_rect[1,0] + ychr_max * 1.5
   lonlat_rect[0,2] = lonlat_rect[0,0] + xchr_max * 27
   lonlat_rect[1,2] = lonlat_rect[1,1]
   lonlat_rect[0,3] = lonlat_rect[0,2]
   lonlat_rect[1,3] = lonlat_rect[1,0]
   lonlat_rect[0,4] = lonlat_rect[0,0]
   lonlat_rect[1,4] = lonlat_rect[1,0]

   ;------------------------------------------------------------------------
   ; If we're only drawing block outlines, print title.
   ;------------------------------------------------------------------------

   font_size = (yscr_max GT 1000) ? '18' : '14'

   SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:font_size, Face:'bold'}
   char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.5 : 1.5

   IF (PathOrOrbit EQ 1) THEN BEGIN
      XYOUTS, 2, yview_max - 2.5 * ychr_max, '  Path number = ' + $
              STRTRIM(STRING(PathNum),2), COLOR=!KON.Colors.black, $
              /DEVICE, CHARSIZE=1.2
   ENDIF ELSE BEGIN
                                ; 98.88 min/orbit => 14.5631 orbits/day
      orbit_per_day = !KON.Instr.NUM_PATHS / 15.9993
      baseorbit = 1007.281      ; first orbit at noon (Julian) on basedate
      basedate  = '02/25/00'    ; day after final orbit reached (at noon)
      mmddyr    = Is_Date(basedate)
      base_julian = JULDAY(mmddyr[0], mmddyr[1], mmddyr[2])
      orbitjulian = base_julian + (OrbitNum - baseorbit) / orbit_per_day
      CALDAT, orbitjulian, month, day, year, hour, min, sec
      orbitdate = STRING(STRTRIM(year,2))  + '-' + $
                  STRING(STRTRIM(month,2)) + '-' + $
                  STRING(STRTRIM(day,2))
      XYOUTS, 2, yview_max - 1.5 * ychr_max, $
          '  Orbit Number = ' + STRTRIM(STRING(OrbitNum),2) + $
          ';  Path Number = ' + STRTRIM(STRING(PathNum),2), $
          /DEVICE, CHARSIZE=1.3, COLOR=!KON.Colors.black

      BlockNumber = 90.5
      JulianTime  = -1.0D
      Get_OrbitTime_OrbitNumber, JulianTime, 0, OrbitNum, BlockNumber, status
      
      date_gmt = STRING(FORMAT='(C(CYI4.4,"-",CMoA3, "-",CDI2.2))', JulianTime)
      time_gmt = STRING(FORMAT='(C(CHI2.2,":",CMI2.2,":",CSI2.2))', JulianTime)

      XYOUTS, 2, yview_max - 3.0 * ychr_max, /DEVICE, CHARSIZE=1.3, $
              '  Date = ' + date_gmt + ';  GMT Time = ' + time_gmt + $
              ' at Equator', COLOR=!KON.Colors.black
   ENDELSE

   lat = 0
   lon = 0
   aryBlockNumbers = 0
   lonlatdat   = 0
   xygrid = 0
   llgrid = 0
   xysave0 = 0
   xysave1 = 0
   xycoords = 0

   ;------------------------------------------------------------------------
   ; Loop waiting for mouse input.
   ;------------------------------------------------------------------------
   
   SafeWSET, map_wndw, didit
   !MOUSE.button = 1
   xpoint_old = 0
   ypoint_old = 0
   
   WHILE (!MOUSE.button NE 4) DO BEGIN
   
      CURSOR, xpoint, ypoint, /DEVICE, /DOWN

      ;---------------------------------------------------------------------
      ; If user clicks red delete window button in upper-left, exit safely
      ; here. The window has already been destroyed by IDL. The only bad
      ; side-effect is if the user clicks the identical point in the window
      ; twice in succession, this code assumes the window is to be closed.
      ;---------------------------------------------------------------------

      IF (xpoint EQ xpoint_old AND ypoint EQ ypoint_old) THEN BREAK
      IF (!D.WINDOW EQ -1) THEN BREAK

      ;---------------------------------------------------------------------
      ; Fabricate left cursor button click position in lat/long or get it
      ; from mouse and display lat/long coords.
      ;---------------------------------------------------------------------

      xycoords = CONVERT_COORD(xpoint, ypoint, /DEVICE, /TO_DATA)
      Xlon = xycoords[0,0]
      Ylat = xycoords[1,0]
      PLOTS, [xpoint-3,xpoint+3], [ypoint,ypoint], /DEVICE, $
             COLOR=!KON.Colors.black
      PLOTS, [xpoint,xpoint], [ypoint-3,ypoint+3], /DEVICE, $
             COLOR=!KON.Colors.black
      POLYFILL, lonlat_rect, COLOR=!KON.Colors.yellow, /DEVICE   
      PLOTS, lonlat_rect, /DEVICE, COLOR=!KON.Colors.black, THICK=1
      llstring = 'Lat = ' + STRTRIM(STRING(Ylat,FORMAT='(f7.3)'),2) + $
              ',  Long = ' + STRTRIM(STRING(Xlon,FORMAT='(f8.3)'),2)

      SetFontInfo, {Type:!KON.FontTyp.FONT_DEVICE, Size:'12', Face:'bold'}
      char_spac = (!P.FONT EQ !KON.FontTyp.FONT_DEVICE) ? 1.0 : 1.0

      yboot = (!KON.Misc.MINX_PLATFORM EQ 2) ? 3 : 4

      XYOUTS, lonlat_rect[0,0]+4, lonlat_rect[1,0]+yboot, llstring, $
              /DEVICE, COLOR=!KON.Colors.black, CHARSIZE=1.0, ALIGNMENT=0.0

      ;---------------------------------------------------------------------
      ; Test if the mouse click is inside the save image button; if it is,
      ; save the image to a file in PNG format; otherwise return to menu
      ; for another case.
      ;---------------------------------------------------------------------

      IF ((xpoint GE xstart_exit AND $
           xpoint LE xstart_exit+xsize_exit AND $
           ypoint LE ystart_exit AND $
           ypoint GE ystart_exit-ysize_exit)) THEN BEGIN

         SetFontInfo, old_font
         !MOUSE.X = 0
         !MOUSE.Y = 0
         RETURN
      ENDIF         
  
      IF ((xpoint GE xstart_save AND $
           xpoint LE xstart_save+xsize_save AND $
           ypoint LE ystart_save AND $
           ypoint GE ystart_save-ysize_save)) THEN BEGIN

         retval = ImageToPngFile (map_wndw, batch_png_file, OrbitNum, $
                                  xstart_save, ystart_save, $
                                  xsize_save,  ysize_save,  $
                                  xstart_exit, ystart_exit, $
                                  xsize_exit,  ysize_exit,  $
                                  lonlat_rect[0,0],  lonlat_rect[1,1], $
                                  lonlat_rect[0,2] - lonlat_rect[0,0], $
                                  lonlat_rect[1,1] - lonlat_rect[1,0], $
                                  llstring)
         !MOUSE.button = 4
      ENDIF

      xpoint_old = xpoint
      ypoint_old = ypoint
   ENDWHILE

   ;------------------------------------------------------------------------
   ; Clean up.
   ;------------------------------------------------------------------------

   lonlat_rect = 0
   xycoords = 0
   !MOUSE.X = 0
   !MOUSE.Y = 0

   SetFontInfo, old_font

END  ;  DisplayOrbitLocation
