#'@title plot_region_mask
#'
#'@description Plot the shape of the region to use as mask in `plot_region_wordcloud()`.
#'
#'@param df Dataframe for a given region obtained through [`gisco_get_nuts()`](https://rdrr.io/cran/giscoR/man/gisco_get_nuts.html), filtered by its NUTS id.
#'@param bbox The bounding box of the region.
#'
#'@return A ggplot object with the shape of the nuts region.
#'
#'@examples
#'\dontrun{
#'library(giscoR)
#'library(magrittr)
#'df <- giscoR::gisco_get_nuts(nuts_id = "DE2", resolution = "10")
#'bbox_region <- df %>% st_bbox()
#'plot_region_mask(df, bbox_region)
#'}
#'
#'@import ggwordcloud
#'@import ggthemes
#'
#'@export
plot_region_mask <- function(df, bbox){
  ggplot(df) +
    geom_sf(fill = "black", color = "black") + # this sets the color of the polygon

    # set size of plot according to bbox coordinates
    xlim(bbox[c("xmin", "xmax")]) +
    ylim(bbox[c("ymin", "ymax")]) +
    theme_map() +

    # remove all non-word elements of graph
    theme(axis.ticks = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          plot.margin = grid::unit(c(0,0,0,0), "mm"))
}
