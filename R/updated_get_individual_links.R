#
date_append = gsub(pattern = "[[:punct:]]", replacement = '_', x = Sys.Date())
# date_append
dir.create(
  paste('./data-raw',
        date_append, sep = '/'
  ), recursive = T
)
# dir.create('./data-raw')
# importing packages 
library(httr) 
library(XML) 
library(tidyverse)
library(rvest)

# get all links
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
# get links to Gazette
links_df = data.frame()
length_year_df = length(links_tables_1)
for (i in 1:length_year_df){
  cat(i, length_year_df, '\n')
  url2 = links_tables_1[i]
  link2_Read <- read_html(url2)
  #
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
# create path
make_path <- function(path){
  dir.create(dirname(path = path), recursive = TRUE, showWarnings = F);
  path
}
# download files
length_files = length(links_df_2$links2)
incompletes_files = data.frame()
for (url_index in 1:length_files) {
  print(url_index)
  url_str <- links_df_2$links2[url_index]
  dest <- links_df_2$path[url_index]
  dest = make_path(dest)
  #
  # expected_file_size <- httr::HEAD(url_str)$headers$`content-length`
  # disk_file_size <- file.size(dest_file)
  if (!file.exists(dest)) {
    tryCatch(
      download.file(
        url = url_str, 
        destfile = dest, 
        quiet = F, mode = 'wb'), 
      error = function(e) {print('broken')}
    )
  } else {
    # expected_file_size <- httr::HEAD(url_str)$headers$`content-length`
    # dest_files_size = file.size(dest)
    # if (expected_file_size != dest_files_size){
    #   incompletes_files = bind_rows(  
    #     incompletes_files,
    #     bind_cols(
    #       links_df[url_index,], data.frame(destination = dest)
    #       )
    #   )
    #   print(paste("INCOMPLETE: Downloading again", dest, sep = " "))
    #   #
    #   tryCatch(
    #     download.file(
    #       url = url_str,
    #       destfile = dest,
    #       quiet = F, mode = 'wb'),
    #     error = function(e) {print('broken')}
    #   )
    # 
    # } 
    # else {
      print(url_index)
      next
    # }
  }
}
# 4231 Kenya Gazette Vol. XC-No. 19
#

# dir.create('pdf_files/')
# setwd('pdf_files/')
#
# download.file(
#   "https://new.kenyalaw.org/akn/ke/officialGazette/2024-12-20/226/eng@2024-12-20/source",
#   destfile = "pdffile.pdf", 
#   quiet = F, mode = 'wb')
# #)
date_append = gsub(pattern = "[[:punct:]]", replacement = '_', x = Sys.Date())
date_append
save.image(
  paste('./workspaces/',
        date_append, '.RData',sep = ''
  )
)
#