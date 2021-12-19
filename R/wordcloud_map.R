#'@title wordcloud_map
#'
#'@description Plot a map with wordclouds for the selected NUTS level and its regions.
#'
#'@param dataframe 	The dataframe containing the input data.
#'@param country The corresponding ISO 3166-2 code of the chosen country. See wikipedia.org/wiki/ISO_3166-1_alpha-3 for a list of all country codes.
#'@param level_nuts The NUTS level to which the NUTS codes correspond. Must be either `1`, `2` or `3`.
#'@param name_column_words The name of the column in `dataframe` containing the words.
#'@param name_column_frequency The name of the column in `dataframe` containing the frequencies.
#'@param name_column_nuts The name of the column in `dataframe` containing the NUTS codes.
#'@param max_word_size The maximum size of the words in the wordcloud. At the minimum value `1` all the words are equally sized. Default is `4`.
#'@param rm_outside Whether to remove words that could not be fitted in the wordcloud area. If set to `FALSE`, these words will be stacked on top of each other at the centre of each region. Default is `TRUE`.
#'@param scale The desired scale of the regions to be used as the shape of the wordcloud. Must be either `"03"`, `"10"`, `"20"` or `"60"`. Default is `"10"`.
#'@param png_path Path where the png image will be saved to, keeping the original aspect ratio of the country. Default is `"False"`.
#'
#'@return A ggplot object with the wordclouds for each region present in `dataframe`.
#'
#'@examples
#'\dontrun{
#'companies_DEU <- data("companies_DEU")
#'wordcloud_map(companies_DEU, "DEU", 1, "name", "employees", "code")
#'
#'companies_ITA <- data("companies_ITA")
#'wordcloud_map(companies_ITA, "ITA", 2, "name", "employees", "code", max_word_size = 10, rm_outside = F)
#'}
#'
#'@import sf
#'@importFrom giscoR gisco_get_nuts
#'@import ggplot2
#'@import ggthemes
#'@importFrom magrittr %>%
#'@import dplyr
#'@import magick
#'@import png
#'@importFrom grDevices as.raster
#'
#'@export
wordcloud_map <- function(dataframe, country, level_nuts, name_column_words, name_column_frequency, name_column_nuts, max_word_size = 4, rm_outside = TRUE, scale = '10', png_path = 'False'){

  # retrieve dataset of NUTS geometric shapes
  df <- giscoR::gisco_get_nuts(country = country, resolution = scale) %>%
    filter(LEVL_CODE == level_nuts)

  # retrieve bbox for the country
  bbox_country <- gisco_get_nuts(country = country, resolution = scale) %>%
    filter(LEVL_CODE == 0) %>%
    st_bbox()
  width_country <- bbox_country[["xmax"]] - bbox_country[["xmin"]]
  height_country <- bbox_country[["ymax"]] - bbox_country[["ymin"]]

  # create vector with all NUTS IDs
  nuts_ids <- unique(dataframe[[name_column_nuts]])

  # create empty canvas to which wordclouds will be mapped
  p <- ggplot(data.frame()) +
    geom_point() +
    xlim(bbox_country[["xmin"]], bbox_country[["xmax"]]) +
    ylim(bbox_country[["ymin"]], bbox_country[["ymax"]]) +
    theme_void()

  # create empty lists to be used later
  plots <- list()
  list_bbox <- list()

  # create temporary file
  tmp_file_path <- tempfile(fileext = ".png")

  # create png image for each NUTS ID
  for (nuts_id in nuts_ids) {

    df_nuts_id <- filter(df, NUTS_ID == nuts_id)
    bbox_region <- df_nuts_id %>% st_bbox()

    list_bbox[[nuts_id]] <- bbox_region # create a named list with individual bboxes, key is nuts_id

    plot <- plot_region_mask(df_nuts_id, bbox_region)

    # calculate ratio of bbox to use when saving as image
    width <- list_bbox[[nuts_id]][["xmax"]] - list_bbox[[nuts_id]][["xmin"]]
    height <- list_bbox[[nuts_id]][["ymax"]] - list_bbox[[nuts_id]][["ymin"]]
    ratio <- width/height

    # save image to be used as mask
    ggsave(filename = tmp_file_path, plot = plot, width = 1000*ratio, height = 1000*(1/ratio), units = "px") # height and width are in pixels
    image_trimmed <- image_trim(image_read(tmp_file_path))
    image_write(image_trimmed, path = tmp_file_path, format = "png")

    # select data for given region in dataframe
    dataframe_region <- dataframe %>%  filter(code == nuts_id)

    # Create the ggplot
    plots[[nuts_id]] <- plot_region_wordcloud(dataframe_region, tmp_file_path, dataframe_region[[name_column_words]], dataframe_region[[name_column_frequency]], max_word_size, rm_outside)

    # Save wordcloud images to disk
    ggsave(filename = tmp_file_path, plot = plots[[nuts_id]], width = image_info(image_trimmed)[["width"]], height = image_info(image_trimmed)[["height"]], units = "px")

    # crop wordcloud image (for better fitting) and convert to raster
    crop_width <- image_info(image_trimmed)[["width"]]-70
    crop_height <- image_info(image_trimmed)[["height"]]-70
    image <- image_crop(image_read(tmp_file_path), paste0(crop_width, "x", crop_height, "+35+35+35+35"))
    raster_image <- grDevices::as.raster(image) # converts image to raster

    # update canvas with the created wordcloud
    p <- p + annotation_raster(raster_image,
                               list_bbox[[nuts_id]][["xmin"]],
                               list_bbox[[nuts_id]][["xmax"]],
                               list_bbox[[nuts_id]][["ymin"]],
                               list_bbox[[nuts_id]][["ymax"]]
    )

    print(paste("Plotted wordcloud for region", nuts_id))

  }

  # Delete temporary file
  unlink(tmp_file_path)

  if (png_path != 'False'){
    ggsave(p, filename = png_path, width = 400*width_country, height = 400*height_country, units = "px", limitsize = FALSE)
  }

  print("Finished creating wordcloud map")

  # return plot
  p
}
