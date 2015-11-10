plot.platemap <- function(infile,
                          bioneer_samples_1536,
                          file_number,
                          PDF_OUTFILE,
                          excluded_colonies)
{
  empty_spots_1536 = which(bioneer_samples_1536 %in% 'empty')
  empty_spots_1536 = empty_spots_1536[which(! empty_spots_1536 %in%
                                              excluded_colonies)]

  pinning_error_1536 = which(bioneer_samples_1536 %in% 'zero')
  pinning_error_1536 = pinning_error_1536[which(! pinning_error_1536 %in%
                                                  excluded_colonies)]

  more_plate_median_1536 = which(bioneer_samples_1536 %in% 'more')
  more_plate_median_1536 = more_plate_median_1536[which(! more_plate_median_1536 %in%
                                                          excluded_colonies)]

  less_plate_median_1536 = which(bioneer_samples_1536 %in% 'less')
  less_plate_median_1536 = less_plate_median_1536[which(! less_plate_median_1536 %in% excluded_colonies)]

  moresig_plate_median_1536 = which(bioneer_samples_1536 %in% 'moresig')
  moresig_plate_median_1536 = moresig_plate_median_1536[which(! moresig_plate_median_1536 %in% excluded_colonies)]

  lessig_plate_median_1536 = which(bioneer_samples_1536 %in% 'lessig')
  lessig_plate_median_1536 = lessig_plate_median_1536[which(! lessig_plate_median_1536 %in% excluded_colonies)]

  red = empty_spots_1536
  black = pinning_error_1536
  violet = more_plate_median_1536
  green = less_plate_median_1536
  cyan = moresig_plate_median_1536
  yellow = lessig_plate_median_1536
  blue = excluded_colonies

  all_color = 1:nrow(bioneer_samples_1536)
  total_colonies = (length(red)   +
                    length(black) +
                    length(violet)+
                    length(green) +
                    length(cyan)  +
                    length(yellow)+
                    length(blue))

  if (total_colonies == length(bioneer_samples_1536)) {
    if (length(red) > 0) {
      all_color[which(all_color %in% red)] = 2000
    }
    if (length(black) > 0) {
      all_color[which(all_color %in% black)] = 3000
    }
    if (length(violet) > 0) {
      all_color[which(all_color %in% violet)] = 4000
    }
    if (length(green) > 0) {
      all_color[which(all_color %in% green)] = 5000
    }
    if (length(cyan) > 0) {
      all_color[which(all_color %in% cyan)] = 6000
    }
    if (length(yellow) > 0) {
      all_color[which(all_color %in% yellow)] = 7000
    }
    if (length(blue) > 0) {
      all_color[which(all_color %in% blue)] = 8000
    }
  } else {
    stop(paste('total colonies do not add upto the total values in the given file',
               sep = ''))
  }

  all_color[which(all_color %in% 2000)] = 1
  all_color[which(all_color %in% 3000)] = 2
  all_color[which(all_color %in% 4000)] = 3
  all_color[which(all_color %in% 5000)] = 4
  all_color[which(all_color %in% 6000)] = 5
  all_color[which(all_color %in% 7000)] = 6
  all_color[which(all_color %in% 8000)] = 7

  rowvar = rep (c(LETTERS, c(t(outer(LETTERS[1], LETTERS[1:6], paste, sep = "")))), 48)

  columnvar = rep (1:48, each = 32)

  colorvar = all_color

  platelay = data.frame (rowvar, columnvar, colorvar)

  PDF_OUTFILE = paste(PDF_OUTFILE, infile, ".pdf", sep = '_')

  pdf(file = PDF_OUTFILE, width = 13.5, height = 9)

  rowvar = unique(platelay$rowvar)

  columnvar = unique(platelay$columnvar)

  par(xpd = TRUE)

  plot(
    NA,
    ylim = c(0.5,length(rowvar) + 0.5),
    xlim = c(0.5,length(columnvar) + 0.5),
    ann = FALSE,
    axes = FALSE)

  box()

  axis(2,at = seq_along(rowvar),labels = rev(rowvar),las = 2, cex.axis = 1.2)
  axis(3,at = seq_along(columnvar),labels = columnvar, cex.axis = 1.2)

  colgrp = findInterval(platelay$colorvar, sort(as.vector(unique(all_color))))
  legend_posX = 6
  legend_posY = -1.5
  legend_symbol_size = 1.8
  legend_text_size = 1.2
  legend_text = c('Empty',
                  'Pinning Error',
                  'Morethan Plate Median',
                  'Lessthan Plate Median',
                  'Morethan 90% Plate Median',
                  'Lessthan 25% Plate Median',
                  'Excluded Colonies')

  color_vec = c("red", "black", "#660066", 'green', 'cyan', 'yellow', 'blue')

  if (length(all_color) > 0) {
    all_color_uniq_sort = sort(unique(all_color))
    colfunc = colorRampPalette(color_vec[all_color_uniq_sort])
    collist = colfunc(length(unique(colgrp)))
    len_all_color_uniq_sort = length(all_color_uniq_sort)

    if(len_all_color_uniq_sort == 7){
      legend_text = c(legend_text[all_color_uniq_sort], '')
      symbol_type = c(rep(21, len_all_color_uniq_sort),0)
      n_legend_col = 4
      text_width = c(0, 0, 5, 5, 7, 7, 8.5, 8.5)
      pt_cex = c(rep(legend_symbol_size, len_all_color_uniq_sort), 0)
    } else if(len_all_color_uniq_sort == 6){
      legend_text = legend_text[all_color_uniq_sort]
      symbol_type = rep(21, len_all_color_uniq_sort)
      n_legend_col = 3
      text_width = c(0, 0, 8.5, 8.5, 10.5, 10.5)
      pt_cex = rep(legend_symbol_size, len_all_color_uniq_sort)
    } else if(len_all_color_uniq_sort == 5){
      legend_text = c(legend_text[all_color_uniq_sort], '')
      symbol_type = c(rep(21, len_all_color_uniq_sort),0)
      n_legend_col = 3
      text_width = c(0, 0, 8.5, 8.5, 10.5, 10.5)
      pt_cex = c(rep(legend_symbol_size, len_all_color_uniq_sort), 0)
    } else if(len_all_color_uniq_sort == 4){
      legend_text = c(legend_text[all_color_uniq_sort], '', '')
      symbol_type = c(rep(21, len_all_color_uniq_sort), 0, 0)
      n_legend_col = 3
      text_width = c(0, 0, 8.5, 8.5, 10.5, 10.5)
      pt_cex = c(rep(legend_symbol_size, len_all_color_uniq_sort), 0, 0)
    } else if(len_all_color_uniq_sort == 3){
      legend_text = c(legend_text[all_color_uniq_sort], '')
      symbol_type = c(rep(21, len_all_color_uniq_sort), 0)
      n_legend_col = 2
      text_width = c(0, 0, 8.5, 8.5, 10.5, 10.5)
      pt_cex = c(rep(legend_symbol_size, len_all_color_uniq_sort), 0)
    }

    legend(
      x = legend_posX,
      y = legend_posY,
      legend = legend_text,
      ncol = n_legend_col,
      pch = symbol_type,
      pt.bg = collist,
      cex = legend_text_size,
      bty = 'n',
      pt.cex = pt_cex,
      text.width = text_width)

    symbols(platelay$columnvar,
            factor(platelay$rowvar,
                   levels = rev(c(LETTERS, c(t(outer(LETTERS[1],
                                                     LETTERS[1:6],
                                                     paste, sep = "")))))),
            circles = rep(0.4,nrow(platelay)),
            add = TRUE,
            inches = FALSE,
            bg = collist[colgrp])

    title_name = toupper(infile)

    title(main = paste("Fig ",
                       file_number,
                       " : ",
                       title_name,
                       sep = ""),
          line = +2.4)

    # mtext(infile, side = 1,line = +3,at = 22)  #footnote
  }

  dev.off()
}

