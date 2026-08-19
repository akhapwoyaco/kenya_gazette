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
for (url_index in 1:length_files) { #4418
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
  # } else {
  #   expected_file_size <- httr::HEAD(url_str)$headers$`content-length`
  #   dest_files_size = file.size(dest)
  #   if (expected_file_size != dest_files_size){
  #     incompletes_files = bind_rows(
  #       incompletes_files,
  #       bind_cols(
  #         links_df[url_index,], data.frame(destination = dest)
  #         )
  #     )
  #     print(paste("INCOMPLETE: Downloading again", dest, sep = " "))
  #     #
  #     tryCatch(
  #       download.file(
  #         url = url_str,
  #         destfile = dest,
  #         quiet = F, mode = 'wb'),
  #       error = function(e) {print('broken')}
  #     )
  # 
  #   }
  #   else {
  #     print(url_index)
  #     next
  #   }
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

#' Save links_df_2 to a dated folder as CSV and RDS
#'
#' Creates a folder under ./data-raw named after today's date (punctuation
#' replaced with underscores), then writes links_df_2 into that folder
#' twice: once as .csv (human-readable, easy to inspect/share) and once as
#' .rds (preserves R data types exactly, e.g. Date columns, factors, lists).
#' Both filenames are stamped with the current date-time so repeated runs
#' on the same day don't overwrite each other.

# --- Build folder path -------------------------------------------------

# Today's date, e.g. "2026-07-29" -> "2026_07_29"
# date_append <- gsub(pattern = "[[:punct:]]", replacement = '_', x = Sys.Date())

# Target folder: ./data-raw/2026_07_29
out_dir <- paste('./data-raw', date_append, sep = '/')

# Create the folder (and any missing parent dirs).
# showWarnings = FALSE avoids an error/warning if it already exists.
dir.create(out_dir, recursive = TRUE, showWarnings = TRUE)

# --- Build timestamp for filenames --------------------------------------
# Current date + time, e.g. "2026-07-29 14:32:10" -> "2026_07_29_14_32_10"
# (the |\\s+ part also replaces the space between date and time)
datetime_append <- gsub(pattern = "[[:punct:]]|\\s+", replacement = '_', x = Sys.time())

# --- Save as CSV ---------------------------------------------------------
# row.names = FALSE avoids writing an extra unnamed index column
out_file_csv <- file.path(out_dir, paste0('links_df_2_', datetime_append, '.csv'))
write.csv(links_df_2, out_file_csv, row.names = FALSE)

# --- Save as RDS -----------------------------------------------------------
# Keeps exact R types (Date, factor, list-columns, etc.) that CSV would flatten
out_file_rds <- file.path(out_dir, paste0('links_df_2_', datetime_append, '.rds'))
saveRDS(links_df_2, out_file_rds)

# --- (optional) confirm where files went ---------------------------------
# cat("Saved:\n -", out_file_csv, "\n -", out_file_rds, "\n")
