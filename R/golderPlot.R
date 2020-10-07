#' @title golderPlot
#' 
#' @description
#' \code{golderPlot} takes data frame from \code{surfReader} produces plot of surface roughness length vs. 1/Monin Obukhov Length 
#'
#' @export 

golderPlot = function(surface_file_1){
requireNamespace(plyr)
requireNamespace(dplyr)  
  surface_file_1 <- surface_file_1[!is.na(surface_file_1$surface_roughness) & !is.na(surface_file_1$monin_obukhov_length),]
  with(surface_file_1, 
             plot(1/monin_obukhov_length, log10(surface_roughness), ylim=rev(range(log10(surface_roughness))), pch="."))
  
  


  y1 <- min(log10(surface_file_1$surface_roughness))
  y2 <- max(log10(surface_file_1$surface_roughness))
  
  PS_class_df = data.frame(PG = c("A","B","C", "D", "E","F"),
                           a = c(-0.096,-0.037,-0.002, 0, 0.004,0.035),
                           b = c(0.029,0.029,0.018, 0, -0.018,-0.036),
                           y1=y1,
                           y2=y2)
  
  ys <- seq(y1,y2, 0.01)

  calc_invL = function(a,b, y1,y2){
    ys <- seq(y1,y2, 0.01)
    a+b*ys
  }
  
  plot_pgL = function(dfx){
    lines(dfx$inv_L, ys)
    text(median(dfx$inv_L), median(ys), dfx$PG[1])
  }

  line_lx <- plyr::dlply(PS_class_df, .(PG), function(dfx) calc_invL(dfx$a,dfx$b, dfx$y1, dfx$y2))
  
  line_df <- plyr::ldply(line_lx, function(lx) data.frame(inv_L=lx))
  
  
  plyr::d_ply(line_df, .(PG), function(dfx) plot_pgL(dfx))
  return(NULL)
}
