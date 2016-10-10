sens_spec_raw_count_table_draw <- function(row_names_titles,
                                           is_neighborful = TRUE)
{
  row_names <- c('Pinning Error Absent',
                 'Pinning Error Present',
                 'Total')

  dir_var <- dir_setup(is_neighborful)

  neighborless_raw_count <- readRDS(paste(dir_var$pinn_dir10,
                                          list.files(dir_var$pinn_dir10),
                                          sep='/'))

  # neighborful
  for(file_name in list.files(dir_var$pinn_dir5)){
    pdf(file = paste(paste(dir_var$pinn_dir2, file_name, sep='/'),
                     '.pdf',
                     sep=''),
        height = 6,
        width = 18.5)

    file_path <- paste(dir_var$pinn_dir5, file_name, sep='/')

    neighborful_raw_count <- readRDS(file_path)

    p1 <- get_gtree_table(data_sens_spec = neighborful_raw_count[1:3,],
                          row_names = row_names,
                          title_text = row_names_titles[1])

    p2 <- get_gtree_table(data_sens_spec = neighborful_raw_count[4:6,],
                          row_names = row_names, 
                          title_text = row_names_titles[2])

    p3 <- get_gtree_table(data_sens_spec = neighborful_raw_count[7:9,],
                          row_names = row_names,  
                          title_text = row_names_titles[3])

    p4 <- get_gtree_table(data_sens_spec = neighborful_raw_count[10:12,],
                          row_names = row_names,
                          title_text = row_names_titles[4])

    p5 <- get_gtree_table(data_sens_spec = neighborful_raw_count[13:15,],
                          row_names = row_names,
                          title_text = row_names_titles[5])

    p6 <- get_gtree_table(data_sens_spec = neighborful_raw_count[16:18,],
                          row_names = row_names,
                          title_text = row_names_titles[6])

    p7 <- get_gtree_table(data_sens_spec = neighborful_raw_count[19:21,],
                          row_names = row_names,
                          title_text = row_names_titles[7])

    p8 <- get_gtree_table(data_sens_spec = neighborful_raw_count[22:24,],
                          row_names = row_names,
                          title_text = row_names_titles[8])
    # neighborless
    p9 <- get_gtree_table(data_sens_spec = neighborless_raw_count, 
                          row_names = row_names,
                          title_text = row_names_titles[9])

    # arrange and draw tables
    grid.arrange(grobs = list(p1, p2, p3, p4, p5, p6, p7, p8, p9), nrow = 3)

    dev.off(dev.cur())
  }

  return(TRUE)
}
