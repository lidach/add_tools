#' @title color_palettes
#'
#' @description provides vector of color values from palette
#'
#' @param name name of color palette. see ...
#'
#' @return vector of colors
#'
#' @export
#'

color_palettes <- function(name = NULL,
                           reverse = FALSE,
                           plot_colors = FALSE) {
  # List of palettes
  palettes <- list()
  palettes$a_palette <- c(
    "#2A363BFF", "#019875FF", "#99B898FF", "#FECEA8FF",
    "#FF847CFF", "#E84A5FFF", "#C0392BFF", "#96281BFF"
  )
  palettes$grand_tour <- c(
    "#418D87FF", "#2B5E3DFF", "#73224EFF", 
    "#AD2E37FF", "#E37D41FF", "#E9A144FF", "#F6EBD1FF"
  )
  palettes$gsea <- c(
    "#4500ACFF" , "#2600D1FF", "#6B58EEFF", "#8787FFFF",
    "#C7C0FFFF", "#D4D4FFFF", "#FFBFE5FF", "#FF8888FF",
    "#FF707FFF", "#FF5959FF", "#EE3F3FFF", "#D60C00FF"
  )
  palettes$hokusai <- c(
    "#6D2F20FF", "#B75347FF", "#DF7E66FF", 
    "#E09351FF", "#EDC775FF", "#94B594FF", "#224B5EFF"
  )
  palettes$kotare <- c(
    "#214D65FF", "#287DABFF", "#E5BF86FF", 
    "#B09771FF", "#624B27FF", "#CACFD0FF"
  )
  palettes$lacroix_berry <- c(
    "#B25D91FF", "#CB87B4FF", "#EFC7E6FF",
    "#1BB6AFFF", "#088BBEFF", "#172869FF"
  )
  palettes$laputa <- c(
    "#14191F", "#1D2645", "#403369",
    "#5C5992", "#AE93BE", "#B4DAE5", "#F0D77B"
  )
  palettes$mint <- c(
    "#E4f1E1", "#B4D9CC", "#89C0B6",
    "#63A6A0", "#448C8A", "#287274", "#0D585F"
  )
  palettes$mononoke <- c(
    "#06141F", "#742C14", "#3D4F7D",
    "#657060", "#CD4F38", "#E48C2A", "#EAD890"
  )
  palettes$nmfs_ocean <- c(
    "#A6D4EC", "#78BEE2", "#4AA8D9", "#1C92CF", "#0072BB",
    "#004295", "#002B7B", "#002467", "#001D55", "#001743"
  )
  palettes$ofrenda <- c(
    "#593722FF", "#834D24FF", "#AB5D26FF", "#C3722AFF", "#E1C473FF", 
    "#E6DAB9FF", "#A4B591FF", "#55804DFF", "#416C39FF", "#2C5724FF"
  )
  palettes$pissaro <- c(
    "#134130FF", "#4C825DFF", "#8CAE9EFF", 
    "#8DC7DCFF", "#508CA7FF", "#1A5270FF", "#0E2A4DFF"
  )
  palettes$ponyo <- c(
    "#4C413FFF", "#5A6F80FF", "#278B9AFF",
    "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF"
  )
  palettes$purp_or <- c(
    "#F9DDDA", "#F2B9C4", "#E597B9",
    "#CE78B3", "#AD5FAD", "#834BA0", "#573B88"
  )
  palettes$rainbow1 <- c(
    "#009392", "#39B185", "#9CCB86",
    "#E9E29C", "#EEB479", "#E88471", "#CF597E"
  )
  palettes$rainbow2 <- c(
    "#9E4058FF", "#C2697FFF", "#D0641EFF", "#E68E54FF",
    "#F9AB0EFF", "#FBC559FF", "#589E40FF", "#7FC269FF",
    "#2C3778FF", "#4151B0FF", "#513965FF", "#785596FF"
  )

  pal <- palettes[[name]]

  if (reverse) pal <- rev(pal)

  # option to plot colors
  if (plot_colors) {
    image(1:(length(pal)), 1, as.matrix(1:length(pal)),
      col = pal,
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )
  }

  # print how many colors in palette
  print(paste0("Length of color palette is ", length(pal)))

  return(pal)
}
