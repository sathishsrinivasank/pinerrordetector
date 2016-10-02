# This source file is an helper code for visualizing new configuration of plate data

# 384
PDF_OUTFILE = 'C:\\Users\\sathish\\Desktop\\pinerrordetector\\data-raw\\colonyarea2.pdf'

colonyarea <- read.table('C:\\Users\\sathish\\Desktop\\pinerrordetector\\data-raw\\colonyarea2.txt',
                     sep='\t',
                     header=TRUE,
                     stringsAsFactors = FALSE)

legend_txt_bg_col <- c('Empty'                     = 'red',
                       'Pinning Error'             = 'black',
                       'Morethan Plate Median'     = '#660066',
                       'Lessthan Plate Median'     = 'green',
                       'Morethan 90% Plate Median' = 'cyan',
                       'Lessthan 25% Plate Median' = 'yellow',
                       'Excluded Colonies'         = 'blue')

pdf(file = PDF_OUTFILE, width = 13, height = 9)

plot_platemap(plateformat = 384,
              plate_data = data.frame(mydata$data_subtypes,
                                      stringsAsFactors = FALSE),
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col)

dev.off(which = dev.cur())

#1536

PDF_OUTFILE = 'C:\\Users\\sathish\\Desktop\\pinerrordetector\\data-raw\\colonyarea3.pdf'

mydata2 <- convert_384_to_1536_data(mydata$data_subtypes)

legend_txt_bg_col <- c('Empty'                     = 'red',
                       'Pinning Error'             = 'black',
                       'Morethan Plate Median'     = 'brown',
                       'Lessthan Plate Median'     = 'green',
                       'Morethan 90% Plate Median' = 'cyan',
                       'Lessthan 25% Plate Median' = 'yellow',
                       'Excluded Colonies'         = 'blue')

pdf(file = PDF_OUTFILE, width = 13, height = 9)
plateformat = 1536
plot_platemap(plateformat = 1536,
              plate_data = data.frame(mydata2,
                                      stringsAsFactors = FALSE),
              data_flow = 'down',
              legend_txt_bg_col = legend_txt_bg_col,
              symbol_size = rep(0.4, plateformat))

dev.off(which = dev.cur())
