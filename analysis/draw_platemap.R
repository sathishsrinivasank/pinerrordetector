
draw_platemap <- function(plateformat,
                          categorized_data,
                          legend_txt_bg_col,
                          data_flow = out_data_flow,
                          symbol_size,
                          is_neighborful = FALSE, ...)
{
  # 1. sanity checks
  stopifnot(is.vector(categorized_data) && is.character(categorized_data))
  
  dir_var <- dir_setup(is_neighborful)

  # draw template platemap
  PDF_TEMPLATE <- paste(sub('/neighborful/platemaps', '', dir_var[[3]]),
                        'template_platemap.pdf',
                        sep = '/')
  plot_data <- plate_coords(plate_to = plateformat,
                            data_from = categorized_data,
                            data_format = data_flow)
  las2 <- 2
  las3 <- 0
  cex_axis <- 1.2
  legend_pch <- 21
  legend_text_width <- NULL
  legend_ncol <- 4
  legend_posX <- 0
  legend_posY <- -1
  legend_symbol_size <- 1.8
  legend_text_size <- 1.2

  pdf(file = PDF_TEMPLATE, width = 13, height = 9)

  plot_platemap(plateformat = plateformat,
                plot_data = plot_data,
                data_flow = data_flow,
                legend_txt_bg_col = legend_txt_bg_col,
                las2 = las2,
                las3 = las3,
                cex_axis = cex_axis,
                legend_pch = legend_pch,
                symbol_size = symbol_size,
                legend_text_width = legend_text_width,
                legend_ncol = legend_ncol,
                legend_posX = legend_posX,
                legend_posY = legend_posY,
                legend_symbol_size = legend_symbol_size,
                legend_text_size = legend_text_size)

#   # draw title
#   title_main <- 'Neighborless_Variation'
#   title_line <- 2.4
#   title(main = title_main, line = title_line)
#
#   # draw footnote
#   footnote_text <- template_infile
#   footnote_side <- 1
#   footnote_line <- 3
#   footnote_at <- 22
#   mtext(text = footnote_text,
#         side = footnote_side,
#         line = footnote_line,
#         at   = footnote_at)
#
  dev.off(which = dev.cur())

  # draw algorithm platemaps
  if(is_neighborful){
    pinn_dirs <- c(dir_var$pinn_dir3, dir_var$pinn_dir8)
    pinn_outdirs <- c(dir_var$pinn_dir1, dir_var$pinn_dir6)
  } else {
    pinn_dirs <- dir_var$pinn_dir8
    pinn_outdirs <- dir_var$pinn_dir6
  }

  for(pinn_dir in pinn_dirs){
    all_file_names <- list.files(pinn_dir)

    if(length(all_file_names) <= 0){
      warning(paste('There are no excluded files in ', pinn_dir, sep=''))
    } else {
      for(file_name in all_file_names){
        file_path <- paste(pinn_dir, file_name, sep='/')

        excluded_all <- readRDS(file_path)

        len_excluded_all <- length(excluded_all)

        if(len_excluded_all <= 0){
          stop('There is no excluded colonies data in this file')
        } else {
          PDF_OUTFILE <- paste(paste(pinn_outdirs[which(pinn_dirs %in% pinn_dir)],
                                     file_name,
                                     sep='/'),
                               '.pdf',
                               sep = '')

          pdf(file = PDF_OUTFILE, width = 13, height = 9)

          for(exc_list_count in 1:len_excluded_all){
            excluded_indices <- excluded_all[[exc_list_count]][[1]]

            plot_data <- categorized_data

            plot_data[excluded_indices] <- 'Excluded Colonies'


            plot_data <- plate_coords(plate_to = plateformat,
                                      data_from = plot_data,
                                      data_format = data_flow)
            las2 <- 2
            las3 <- 0
            cex_axis <- 1.2
            legend_pch <- 21
            legend_text_width <- NULL
            legend_ncol <- 4
            legend_posX <- 0
            legend_posY <- -1
            legend_symbol_size <- 1.8
            legend_text_size <- 1.2

            plot_platemap(plateformat = plateformat,
                          plot_data = plot_data,
                          data_flow = data_flow,
                          legend_txt_bg_col = legend_txt_bg_col,
                          las2 = las2,
                          las3 = las3,
                          cex_axis = cex_axis,
                          legend_pch = legend_pch,
                          symbol_size = symbol_size,
                          legend_text_width = legend_text_width,
                          legend_ncol = legend_ncol,
                          legend_posX = legend_posX,
                          legend_posY = legend_posY,
                          legend_symbol_size = legend_symbol_size,
                          legend_text_size = legend_text_size)
          }

          dev.off(which = dev.cur())
        }
      }
    }
  }
  return(TRUE)
}



