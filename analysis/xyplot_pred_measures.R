xyplot_pred_measures <- function(is_neighborful = TRUE)
{
  # 1. Setup data
  dir_var <- dir_setup(is_neighborful)

  ful_less_all <- data.frame(stringsAsFactors = FALSE)

  neighborless <- readRDS(file = paste(dir_var$pinn_dir9,
                                       list.files(dir_var$pinn_dir9),
                                       sep = '/'))

  for(param1 in 1:8){
    for(param2 in 1:8){
      file_name <- paste('neighborful_excluded_colonies_A_H_25percent_',
                         param1,
                         '_',
                         param2,
                         '.rds',
                         sep = '')

      file_path <- paste(dir_var$pinn_dir4, file_name, sep='/')

      neighborful <- readRDS(file = file_path)

      ful_less <- do.call('rbind', list(neighborful, neighborless))

      ful_less <- do.call('cbind', list(ful_less,
                                        param1 = rep(param1, nrow(ful_less)),
                                        param2 = rep(param2, nrow(ful_less)),
                                        algo_type = 1:nrow(ful_less)))

      ful_less_all <- do.call('rbind', list(ful_less_all, ful_less))

    }
  }

  strip_names <- c('Sensitivity (%)',
                   'Specificity (%)',
                   'Positive Predictive Value (%)',
                   'Negative Predictive Value (%)')

  final_data <- data.frame(y         = c(ful_less_all$sensitivity,
                                         ful_less_all$specificity,
                                         ful_less_all$ppv,
                                         ful_less_all$npv),
                           test_type = c(rep(strip_names[1], nrow(ful_less_all)),
                                         rep(strip_names[2], nrow(ful_less_all)),
                                         rep(strip_names[3], nrow(ful_less_all)),
                                         rep(strip_names[4], nrow(ful_less_all))),
                           algo_type = rep(ful_less_all$algo_type,
                                           length(strip_names)),
                           param1 = rep(ful_less_all$param1, length(strip_names)),
                           param2 = rep(ful_less_all$param2, length(strip_names)),
                           stringsAsFactors = FALSE)

  for(param2 in 1:8){
    param2_txt <- paste('Exclusion Criteria-2: ', param2, sep='')

    final_data[which(final_data$param2 == param2), 'param2'] <- param2_txt
  }

  # 2. draw xyplot
  colfunc <- colorRampPalette(c("brown",
                                '#FFD700',
                                'blue',
                                'black',
                                '#008B8B',
                                '#FF00FF',
                                'green',
                                '#9400D3',
                                "red"))

  start <- 1

  pset <- list(strip.background=list(col='gray'),
               fontsize = list(text=18))

  CEX <- 1.4

  for(end in c(4,8)){
    a1 <- useOuterStrips(xyplot(x           = y ~ param1 | factor(param2) +
                                                          test_type,
                                data        = final_data,
                                group       = algo_type,
                                jitter.y    = FALSE,
                                layout      = c(4,4),
                                grid        = TRUE,
                                type        = 'o',
                                main        = "",
                                xlab        = list("Exclusion Criteria-1",
                                                  cex = CEX),
                                ylab        = NULL,
                                key         = list(text = list(LETTERS[1:9],
                                                              col = 'black',
                                                              cex = CEX),
                                                  lines = list(col = colfunc(9),
                                                               lwd = 1,
                                                               pch = 1:9,
                                                               cex = CEX,
                                                               type = 'b'),
                                                  space = "right",
                                                  columns = 1),
                                pch         = 1:9,
                                cex         = CEX,
                                lwd         = 1.2,
                                scales      = list(x=list(tick.number = 8,
                                                          cex = CEX),
                                                    y=list(tick.number = 6,
                                                          cex = CEX)),
                                col         = colfunc(9),
                                index.cond  = list(start:end, c(1, 2, 4, 3)),
                                par.settings = pset,
                                panel = panel.superpose),
                         strip.left       = strip.custom(horizontal = FALSE),
                         strip.lines      = 1.5,
                         strip.left.lines = 1.5)

    pdf_file <- paste(sub('/raw_count', '', dir_var$pinn_dir2),
                    '/xyplot_', start, '_', end,
                    '.pdf',
                    sep = '')

    trellis.device(pdf, file = pdf_file, height = 16, width = 18)

    plot(a1)

    dev.off(dev.cur())

    start <- end + 1
  }

  return(TRUE)
}

