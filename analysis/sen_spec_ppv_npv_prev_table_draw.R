sen_spec_ppv_npv_prev_table_draw <- function(row_names_titles,
                                             is_neighborful = TRUE)
{
  dir_var <- dir_setup(is_neighborful)

  neighborless_sen_spec_all <- readRDS(paste(dir_var$pinn_dir9,
                                             list.files(dir_var$pinn_dir9),
                                             sep='/'))

  # neighborful
  for(file_name in list.files(dir_var$pinn_dir4)){

    file_path <- paste(dir_var$pinn_dir4, file_name, sep='/')

    neighborful_sen_spec_all <- readRDS(file_path)

    sen_spec_all <- do.call('rbind', list(neighborful_sen_spec_all,
                                          neighborless_sen_spec_all))

    colnames(sen_spec_all) <- c('Sensitivity',
                                'Specificity',
                                'Positive Predictive Value',
                                'Negative Predictive Value',
                                'Prevalence')

    row.names(sen_spec_all) <- row_names_titles

    file_save_path <- paste(dir_var$pinn_dir7, file_name, sep='/')

    pdf(file = paste(file_save_path, '.pdf', sep=''),
        height = 3,
        width = 9.5)

    grid.table(sen_spec_all)

    dev.off(dev.cur())
  }
  return(TRUE)
}
