# kenya_gazette

Text Data Analysis of kenya gazette

## [2018](2018) \| [2019](2019) \| [2020](2020) \| [2021](2021) \| [2022](2022) \| [2023](2023) \| [2024](2024) \| [2025](2025)

## Notable Mentions

General Elections Kenya Analysis and Shiny Dashboards

+ https://akhapwoyachris.shinyapps.io/presidentialelectionkenya2022/
+ https://christopherakhapwoya.shinyapps.io/electionkenya/
+ [Fiver Services](https://www.fiverr.com/s/jj5dYam)

## [Data Source Scripts](./R/updated_get_individual_links.R)

R File: ./R/updated_get_individual_links.R Steps to download the files

-   get links from tables
-   download all files if they done exist on local
-   if on second run they exist, compare online size and download file and if not equal redownloded
-   second else ensures all files are downloaded irrespective of size comparison

```{r}
# set saving of raw data
date_append = gsub(pattern = "[[:punct:]]", replacement = '_', x = Sys.Date())
# date_append
dir.create(
  paste('./data-raw',
        date_append, sep = '/'
  ), recursive = T
)
#
# importing packages 
library(httr) 
library(XML) 
library(tidyverse)
library(rvest)

# get all links of parent links ie yearly links
url <- "https://new.kenyalaw.org/gazettes/"
file_Read <- read_html(url)
links_tables_1 <- file_Read |> 
  html_nodes(xpath = '//*[@id="top"]/div[2]/div[2]') |> 
  #html_elements('h2') |> 
  html_elements('a') |> 
  html_attr('href') |> 
  str_subset(pattern = "#", negate = T) |>
  paste0('https://new.kenyalaw.org', . = _, sep = '')
#
links_tables_1
#
# get links to Gazette pdf files
links_df = data.frame()
length_year_df = length(links_tables_1)
for (i in 1:length_year_df){
  cat(i, length_year_df, '\n')
  url2 = links_tables_1[i]
  link2_Read <- read_html(url2)
  # it is a table to we scrap table, get the underlying hrefs too
  link2_Read_node = link2_Read |>
    html_nodes(xpath = '//*[@id="doc-table"]')
  table = link2_Read_node |>
    html_table() |>
    (\(x) x[[1]])() |> 
    janitor::clean_names() |> 
    filter(grepl(pattern = "gazette", title, ignore.case = T)) |>
    mutate(across(everything(), .fns = as.character))
  link = link2_Read_node |> 
    html_elements('a') |>
    html_attr('href') |>
    paste0('https://new.kenyalaw.org', . = _, sep = '')
  #
  links_df = bind_rows(
    links_df, 
    cbind(table, data.frame(links = link)))
}
#
View(links_df)

# create path and place to file to save
links_df_2 = links_df |>
  mutate(
    links2 = paste(links, 'source', sep = '/'),
    path = paste(
      "pdf_files",
      format(lubridate::dmy(date), "%Y/%b"),
      paste(
        str_replace_all(
          string = title,
          c("[[:punct:]]" = '', " " = "_")), '.pdf', sep = ''),
      sep = '/')
  )
#
# create path function to enable directory creation during download
make_path <- function(path){
  dir.create(dirname(path = path), recursive = TRUE, showWarnings = F);
  path
}
# download files
length_files = length(links_df_2$links2)
incompletes_files = data.frame() # saves files with problems
for (url_index in 1:length_files) {
  print(url_index)
  url_str <- links_df_2$links2[url_index]
  dest <- links_df_2$path[url_index]
  dest = make_path(dest)
  #
  # download pdf given link and save in file 
  if (!file.exists(dest)) {
    tryCatch(
      download.file(
        url = url_str, 
        destfile = dest, 
        quiet = F, mode = 'wb'), 
      error = function(e) {print('broken')} 
    )
  } else {  # comment out this else on first run
    # use the else if file exists to check if file online is different from local
    # if not download again
    expected_file_size <- httr::HEAD(url_str)$headers$`content-length`
    dest_files_size = file.size(dest)
    if (expected_file_size != dest_files_size){
      incompletes_files = bind_rows(
        incompletes_files,
        bind_cols(
          links_df[url_index,], data.frame(destination = dest)
          )
      )
      print(paste("INCOMPLETE: Downloading again", dest, sep = " "))
      #
      tryCatch(
        download.file(
          url = url_str,
          destfile = dest,
          quiet = F, mode = 'wb'),
        error = function(e) {print('broken')}
      )

    } 
    # else { # if file exists locally skip # use only one else at a time
    #  print(url_index)
    #  next
    # }
  }
}
# save worksapce
date_append = gsub(pattern = "[[:punct:]]", replacement = '_', x = Sys.Date())
date_append
save.image(
  paste('./workspaces/',
        date_append, '.RData',sep = ''
  )
)
```

## [Conversion to text and jpeg for analysis](./R/to_png.R)

```{r}
library(pdftools)
library(magick)
library(tesseract)
#
dir.create('text_files')
dir.create('jpeg_files')
dir.create('text-raw')
#
list_of_files <- list.files("pdf_files/", recursive = T, full.names = T)
len_list_of_files = length(list_of_files)
len_list_of_files
# make path
make_path <- function(path){
  dir.create(dirname(path = path), recursive = TRUE, showWarnings = F);
  path
}
# start conversions
for (pdf_file_index in 1:len_list_of_files) {
  percentum = 100*pdf_file_index/len_list_of_files
  print(percentum)
  pdf_file = list_of_files[pdf_file_index]
  pages_in = pdf_info(pdf = pdf_file)$pages
  pdf_file_text <- tryCatch(
    pdf_text(pdf = pdf_file), 
    error = function(e) {
      write.table(pdf_file, 'broken_links.txt', append = T)
    }
  )
  #
  # jpeg
  png_file_path = gsub(
    x = pdf_file, pattern = "pdf_files/|.pdf", replacement = ""
    ) |> paste('jpeg_files/',sep = '', .=_) |> 
    paste('/',sep = '')#paste('.jpeg', sep = '')
  #
  for (i in 1:pages_in){
    png_file_p = make_path(
      png_file_path |> 
        paste('page', i, '.png', sep = '')
      ) 
    png::writePNG(
      target = png_file_p, 
      pdf_render_page(
        pdf = pdf_file, 
        page = i, dpi = 250), metadata = T)
  }
  # txt file
  text_file_path = gsub(
    x = pdf_file, pattern = "pdf_files/|.pdf", replacement = ""
  ) |> paste('text_files/',sep = '', .=_)
  text_file_name = make_path(text_file_path)  |> 
    paste('.txt', sep = '')
  #
  # all_text
  pdf_file_text <- pdf_text(pdf = pdf_file)
  write.table(pdf_file_text, text_file_name, append = F, col.names = NA)
  # master file
  # write.table(pdf_file_text, 'text-raw/pdf_file_text2.txt', append = T, col.names = NA)
}
#
```
