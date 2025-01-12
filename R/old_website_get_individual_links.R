#
# beforer jan 2024 ----------------------------------------------------------
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
vars = c("href") #Define attributes you want to keep
url <- "https://new.kenyalaw.org/gazettes/"
file_Read <- read_html(url)
links_tables_1 <- file_Read |> 
  html_nodes(xpath = '//*[@id="top"]/div[2]/div[2]') |>
  html_attrs() |>
  lapply(function(x) {unlist(x[vars])})

# extract links leading to gazzetes
links_table_base_url <- links_tables_1 |> unlist() |> as.data.frame() |> 
  setNames(c('Links')) |>
  dplyr::filter(grepl('month', Links))
links_table_base_url
#

# data to hold all
all_data_files <- data.frame(NULL)
#
for (link_address in 1:length(links_table_base_url$Links)){
  print(link_address)
  print(
    paste(
      100*link_address/length(links_table_base_url$Links),
      '%'))
  #
  link_add <- links_table_base_url$Links[link_address]
  #
  file_Read_2 <- read_html(link_add)
  links_tables_2 <- file_Read_2 |> 
    html_table(header = T)
  # links_tables_2
  # Filter(function(x) ncol(x) > 2, links_tables_2) |> 
  #   bind_rows()
  # associated links
  # file_Read_2 |> 
  #   html_nodes(xpath = '//a') |>
  #   html_attr('href')
  #
  row_data <- file_Read_2 |> 
    html_nodes("table") |> 
    html_nodes( "tr")
  #
  links_tables_3 <- lapply( row_data, function(x){
    x |> html_nodes( "a") |> html_attr("href")}) |> 
    unlist() |> as.data.frame() |>
    set_names('PDF_Links')
  # # addd download link
  # links_tables_3 <- links_tables_3 |>
  #   rowwise() |>
  #   dplyr::mutate(
  #     pdf_file_link = PDF_Links |> 
  #       # replace spacing 
  #       str_replace_all(pattern = ' ', '%20') |> 
  #       read_html() |> 
  #       html_nodes(xpath = "//div[@class='sd']/a") |> 
  #       html_attr('href')
  #   )
  # all data 
  all_data_files <- rbind(
    all_data_files,
    cbind(
      Filter(
        function(x) {
          ncol(x) > 2 & nrow(x) != 0
        }, links_tables_2) |> 
        bind_rows(), 
      links_tables_3)
  )
  #
}

#

#
all_data_file_name <- paste(
  './data-raw',
  date_append, 
  "all_data_files.csv",
  sep = '/'
)
all_data_file_name
write.csv(all_data_files, all_data_file_name,
          row.names = F)
# Filter(function(x) ncol(x) > 2 & nrow(x) != 0, links_tables_2) |>
# bind_rows()
# 
# class of download
#

# addd download link
links_data <- all_data_files |> select(2:4) |>
  dplyr::mutate(
    pdf_file_link = # replace spacing 
      str_replace_all(PDF_Links, pattern = ' ', replacement = '%20') 
  )
#
# function to map on the links
get_gazette_hrefs <- function(link) {
  link |>
    GET(timeout(200)) |>
    read_html() |> 
    html_nodes(xpath = "//div[@class='sd']/a") |> 
    html_attr('href')
}
#
# function to map on the links
get_gazette_hrefs_2 <- function(link) {
  return(tryCatch(
    link |>
      GET(timeout(200)) |>
      read_html() |> 
      html_nodes(xpath = "//div[@class='sd']/a") |> 
      html_attr('href')
    , error=function(e) NULL))
}

#
links_data_2_v2 <- links_data |>  
  dplyr::mutate(
    content2 = purrr::map(
      pdf_file_link, get_gazette_hrefs_2, .progress = T)
  )
#
# links_data_21 <- links_data_2
# #
# links_data_21 <- links_data_21 |> 
#   rowwise() |>  
#   mutate(
#     content = unlist(content)
#   )
# dplyr::mutate(
#   content2 = ifelse(
#     is.null(unlist(content)), 
#       purrr::map(
#         pdf_file_link, get_gazette_hrefs_2, .progress = T),
#     content))
#
links_data_2_v3_backup <- links_data_2_v2
links_data_2_v2_content <- unnest(links_data_2_v2, content2)

#
links_data_2_v2_content_file_name <- paste(
  './data-raw',
  date_append, 
  "download_links_data_2_v2_content.csv",
  sep = '/'
)
links_data_2_v2_content_file_name
#
write.csv(
  links_data_2_v2_content,
  links_data_2_v2_content_file_name, 
  row.names = F)
#
links_data_2_v2_content_v2 <- links_data_2_v2_content |> 
  mutate(
    dest_files = paste(Date)
  )
#
links_data_2_v2_content_final <- links_data_2_v2_content |> 
  separate(col = Date, into = c('day', 'month_year'), 
           sep = ' ', remove = F) |> 
  separate(col = month_year, into = c('month', 'year'), 
           sep = ',', remove = T) |> 
  mutate(
    f_path =file.path(year, month),# sep = "/"),
    gazette_vol = paste(str_replace_all(
      string = `Gazette Volume`,
      "[[:punct:]]", ''), '.pdf', sep = '')
  ) |> 
  mutate(
    f_path_2 = paste(
      f_path, 
      gazette_vol, 
      sep = "/")) |> select(content2, f_path, f_path_2)
#
setwd('pdf_files/')
# create path
make_path <- function(path){
  dir.create(dirname(path = path), recursive = TRUE, showWarnings = F);
  path
}
# dir.create('pdf_files')
for (url_index in 1:length(links_data_2_v2_content_final$content2)) {
  print(url_index)
  url_str <- links_data_2_v2_content_final$content2[url_index]
  dest <- links_data_2_v2_content_final$f_path_2[url_index]
  dest = make_path(dest)
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
  } 
}
# content_1 = sapply(
#   X = links_data$pdf_file_link, 
#   FUN = get_gazette_hrefs_2, simplify = T)
# #
# # get_gazette_hrefs(links_data$pdf_file_link[111])
# get_gazette_hrefs(links_data$pdf_file_link[225])
# get_gazette_hrefs(links_data$pdf_file_link[224])
# get_gazette_hrefs(links_data$pdf_file_link[226])
# 
# get_gazette_hrefs(links_data$pdf_file_link[259])
#
links_sep_18_2024 <- links_data_2_v2_content_final$content2
#