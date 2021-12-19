library(stringr)
library(knitr)

update_readme <- function() {

# copy from index.Rmd
file.copy(from = "./index.Rmd", to = "../README.Rmd", overwrite = TRUE)

# read file
x <- readLines("./index.Rmd")

# edit yaml_header
yaml_header <- x[1:18]
yaml_header <- gsub( "html_document:", "github_document", yaml_header )
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = 'subtitle: "A package for creating wordcloud maps in R"')]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "toc: TRUE")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "df_print: paged")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "number_sections: FALSE")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "highlight: tango")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "theme: lumen")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "toc_depth: 3")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "toc_float: true")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "css: custom.css")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "self_contained: false")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "includes:")]
yaml_header <- yaml_header[!str_detect(yaml_header, pattern = "after_body: footer.html")]

# edit text body
text_body <- x[19:(length(x)-7)]
text_body[2] <- "A package for creating wordcloud maps in R"
text_body <- text_body[!str_detect(text_body, pattern="(?<!-)-{3}(?!-)")]
text_body <- gsub( "./", "./docs/", text_body, fixed = T)

# append vectors
final <- c(yaml_header, text_body)

# save
writeLines(final, "../README.Rmd")

}
