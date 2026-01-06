#' AERSURFACE input file template
#'
#' Internal character vector used to generate AERMINUTE input files.
#'
#' @noRd
"landUseValues" <- structure(list(Class.Number = c(11L, 12L, 21L, 22L, 23L, 23L, 
31L, 31L, 32L, 33L, 41L, 42L, 43L, 51L, 51L, 61L, 71L, 81L, 82L, 
83L, 84L, 85L, 91L, 92L), Class.Name = structure(c(11L, 14L, 
9L, 8L, 2L, 2L, 1L, 1L, 15L, 19L, 3L, 5L, 10L, 17L, 17L, 12L, 
7L, 13L, 16L, 18L, 6L, 20L, 21L, 4L), .Label = c("Bare Rock/Sand/Clay", 
"Commerical/Industrial/Transportation", "Deciduous Forest", "Emergent Herbaceous Wetlands", 
"Evergreen Forest", "Fallow", "Grasslands/Herbaceous", "High Intensity Residential", 
"Low Intensity Residential", "Mixed Forest", "Open Water", "Orchards/Vineyards/Other", 
"Pasture/Hay", "Perennial Ice/Snow", "Quarries/Strip Mines/Gravel", 
"Row Crops", "Shrubland", "Small Grains", "Transitional", "Urban/Recreational Grasses", 
"Woody Wetlands"), class = "factor"), Seasonal.Albedo.Value.Summer = c(0.1, 
0.6, 0.16, 0.18, 0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.16, 0.12, 
0.14, 0.25, 0.18, 0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.15, 0.14, 
0.14), Seasonal.Albedo.Value.Autumn = c(0.1, 0.6, 0.16, 0.18, 
0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.16, 0.12, 0.14, 0.25, 0.18, 
0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.15, 0.14, 0.14), Seasonal.Albedo.Value.LateAutumn = c(0.1, 
0.7, 0.18, 0.18, 0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.17, 0.12, 
0.14, 0.25, 0.18, 0.18, 0.2, 0.18, 0.18, 0.18, 0.18, 0.18, 0.14, 
0.14), Seasonal.Albedo.Value.Winter.Snow = c(0.1, 0.7, 0.45, 
0.35, 0.35, 0.35, NA, 0.6, 0.6, 0.45, 0.5, 0.35, 0.42, NA, 0.5, 
0.5, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.3, 0.3), Seasonal.Albedo.Value.Spring = c(0.1, 
0.6, 0.16, 0.18, 0.18, 0.18, 0.2, 0.2, 0.2, 0.18, 0.16, 0.12, 
0.14, 0.25, 0.18, 0.14, 0.18, 0.14, 0.14, 0.14, 0.18, 0.15, 0.14, 
0.14), Reference = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), Arid = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "N", "Y"), class = "factor"), 
    Airport = structure(c(1L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
    ), .Label = c("", "N", "Y"), class = "factor")), .Names = c("Class.Number", 
"Class.Name", "Seasonal.Albedo.Value.Summer", "Seasonal.Albedo.Value.Autumn", 
"Seasonal.Albedo.Value.LateAutumn", "Seasonal.Albedo.Value.Winter.Snow", 
"Seasonal.Albedo.Value.Spring", "Reference", "Arid", "Airport"
), class = "data.frame", row.names = c(NA, -24L))
