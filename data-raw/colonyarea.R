template_infile <- 'data-raw/colonyarea.txt'

colonyarea <- read.table(file = template_infile,
                         header = TRUE,
                         sep = '\t',
                         stringsAsFactors = FALSE)

colonyarea <- colonyarea[, c(2,3,4)]

save(colonyarea, file = 'data/colonyarea.rda', compress = 'xz')

# check optimal compression by checking the file sizes: gzip, xz, bzip2
# tools::checkRdaFiles('data/colonyarea.Rda')

#  devtools::use_data(colonyarea,
#                     pkg = 'pinerrordetector',
#                     overwrite = TRUE,
#                     compress = 'xz')
