#'@title plot_region_wordcloud
#'
#'@description Plot a wordcloud in the shape of a region.
#'
#'@param dataframe 	The dataframe containing the input data for a single region.
#'@param mask The path to a png image of the region to be used as the shape of the wordcloud. Can be retreived by using `plot_region_mask()`.
#'@param name_column_words The name of the column in `dataframe` containing the words.
#'@param name_column_frequency The name of the column in `dataframe` containing the frequencies.
#'@param max_word_size The maximum size of the words in the wordcloud. At the minimum value `1` all the words are equally sized. Default is `4`.
#'@param rm_outside Whether to remove words that could not be fitted in the wordcloud area. If set to `FALSE`, these words will be stacked on top of each other at the centre of each region. Default is `TRUE`.
#'
#'@return A ggplot object with a randomly coloured wordcloud in the shape of the region ind `dataframe`.
#'
#'@examples
#'\dontrun{
#'companies_region <- data("companies_DEU") %>% filter(code == "DE2")
#'plot_region_wordcloud(companies_region, "./mask.png", companies_region[["name"]], companies_region[["employees"]])
#'}
#'
#'@import ggwordcloud
#'@import RColorBrewer
#'@import ggthemes
#'
#'@export
plot_region_wordcloud <- function(dataframe, mask, name_column_words, name_column_frequency, max_word_size = 4, rm_outside = TRUE){ # word and frequency: column of the word that it will be plotted and the relative frequencies
  i <- sample(1:12,1) #select random number
  if (i == 11){
    i = 12
  }
  color = brewer.pal(n = 12, name = 'Paired')[i] # select random color from a palette

  ggplot(dataframe, aes(label = name_column_words, size = name_column_frequency, color = name_column_frequency)) +
    geom_text_wordcloud(
      mask = png::readPNG(mask), # the non transparent pixels in an image array (or the black pixel if there is no transparency) will be used as a mask
      rm_outside = rm_outside # remove the texts that could not be fitted
    ) +
    scale_size(range = c(1, max_word_size)) + # this set the size of words: if too low, words don't take shape - if too high, shape also not clear
    scale_color_gradient(low = color, high = color) +
    theme_minimal()
}
