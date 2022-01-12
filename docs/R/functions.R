#' Add copywrite stamp to image
#'
#' Annotates image with copywrite text and saves new version
#'
#' @param img path to image file
#' @param who string, copywrite owner
#' @param write_to path to write annotated version of the file to. Default: cw
#'   sub-directory of directory containing img
#'
#' @return
#' @export
#'
#' @examples
#' dontrun{
#' copywrite("images/extension.png")
#' }
copywrite <- function (img, 
                       who = "GSD Architecture", 
                       write_to = file.path(here::here(), 
                                            dirname(img), 
                                            "cw", 
                                            basename(img))) {
  
  if (!dir.exists(dirname(write_to))) {
    fs::dir_create(dirname(write_to))
  }

  original <- magick::image_read(file.path(here::here(), img))

  cw <- original %>% 
    magick::image_annotate(text = paste("©", who), 
                           boxcolor = "white", 
                           size = 14)
  # cw
  magick::image_write(cw, write_to)
}

cw_gil <- "© GSD Architecture"
