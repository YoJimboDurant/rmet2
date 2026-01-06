#' AERSURFACE input file template
#'
#' Internal character vector used to generate AERMINUTE input files.
#'
#' @noRd
surfaceTemplate <-
c("CO STARTING", "", "   TITLEONE  !TITLE1! ", "   TITLETWO  !TITLE2!", 
"", "   DEBUGOPT  EFFRAD GRID TIFF", "", "   CENTERLL  !LAT!  !LONG!   NAD83", 
"   ", "   DATAFILE  !LC_TYPE!  \"!LC_FILE!\"", "   DATAFILE  !IMP_TYPE!  \"!IMP_FILE!\"", 
"   DATAFILE  !CNPY_TYPE! \"!CNPY_FILE!\"", "", "   ANEM_HGT   !ANEM_HGT!", 
"", "   CLIMATE   !CLIMATE!   !SNOW!   !ARID!", "  ", "   FREQ_SECT MONTHLY  12  !AP!", 
"", "   SECTOR   1    0   30", "   SECTOR   2   30   60", "   SECTOR   3   60   90", 
"   SECTOR   4   90  120", "   SECTOR   5  120  150", "   SECTOR   6  150  180", 
"   SECTOR   7  180  210", "   SECTOR   8  210  240", "   SECTOR   9  240  270", 
"   SECTOR  10  270  300", "   SECTOR  11  300  330", "   SECTOR  12  330  360", 
"", "   SEASON   WINTERNS  !WINTERNS!", "   SEASON   WINTERWS  !WINTERWS!", 
"   SEASON   SPRING    !SPRING!", "   SEASON   SUMMER    !SUMMER!", 
"   SEASON   AUTUMN    !AUTUMN!", "", "   RUNORNOT  RUN  ", "", 
"CO FINISHED", "", "OU STARTING", "", "   SFCCHAR    \"aersurface.sfc.out\"", 
"   EFFRAD     \"MC_2001_Imp_Can_effrad.dbg\"", "   NLCDGRID   \"MC_2001_Imp_Can_nlcdgrid.dbg\"", 
"   MPRVGRID   \"MC_2001_Imp_Can_mprvgrid.dbg\"", "   CNPYGRID   \"MC_2001_Imp_Can_cnpygrid.dbg\"", 
"   NLCDTIFF   \"MC_2001_Imp_Can_nlcdtiff.dbg\"", "   MPRVTIFF   \"MC_2001_Imp_Can_mprvtiff.dbg\"", 
"   CNPYTIFF   \"MC_2001_Imp_Can_cnpytiff.dbg\"", "", "OU FINISHED"
)
