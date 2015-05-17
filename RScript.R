# Copy the PA1_template.Rmd file to R working directory and run this to generate the markdown and html files
library(knitr)
library(markdown)
knit("PA1_template.Rmd")
markdownToHTML("PA1_template.md", "PA1_template.html")