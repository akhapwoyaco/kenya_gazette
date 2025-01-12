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
#