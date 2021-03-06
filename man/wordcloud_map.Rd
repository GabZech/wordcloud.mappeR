% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wordcloud_map.R
\name{wordcloud_map}
\alias{wordcloud_map}
\title{wordcloud_map}
\usage{
wordcloud_map(
  dataframe,
  country,
  level_nuts,
  name_column_words,
  name_column_frequency,
  name_column_nuts,
  max_word_size = 4,
  rm_outside = TRUE,
  scale = "10",
  png_path = "False"
)
}
\arguments{
\item{dataframe}{The dataframe containing the input data.}

\item{country}{The corresponding ISO 3166-2 code of the chosen country. See wikipedia.org/wiki/ISO_3166-1_alpha-3 for a list of all country codes.}

\item{level_nuts}{The NUTS level to which the NUTS codes correspond. Must be either `1`, `2` or `3`.}

\item{name_column_words}{The name of the column in `dataframe` containing the words.}

\item{name_column_frequency}{The name of the column in `dataframe` containing the frequencies.}

\item{name_column_nuts}{The name of the column in `dataframe` containing the NUTS codes.}

\item{max_word_size}{The maximum size of the words in the wordcloud. At the minimum value `1` all the words are equally sized. Default is `4`.}

\item{rm_outside}{Whether to remove words that could not be fitted in the wordcloud area. If set to `FALSE`, these words will be stacked on top of each other at the centre of each region. Default is `TRUE`.}

\item{scale}{The desired scale of the regions to be used as the shape of the wordcloud. Must be either `"03"`, `"10"`, `"20"` or `"60"`. Default is `"10"`.}

\item{png_path}{Path where the png image will be saved to, keeping the original aspect ratio of the country. Default is `"False"`.}
}
\value{
A ggplot object with the wordclouds for each region present in `dataframe`.
}
\description{
Plot a map with wordclouds for the selected NUTS level and its regions.
}
\examples{
\dontrun{
companies_DEU <- data("companies_DEU")
wordcloud_map(companies_DEU, "DEU", 1, "name", "employees", "code")

companies_ITA <- data("companies_ITA")
wordcloud_map(companies_ITA, "ITA", 2, "name", "employees", "code", max_word_size = 10, rm_outside = F)
}

}
