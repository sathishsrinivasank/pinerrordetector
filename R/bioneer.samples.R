# @method bioneer_samples(bioneer_file_path)
# @param string bioneer_file_path path to gene deletion library - tab delimited file
# @return dataframe gene deletion library
bioneer.samples <- function(bioneer_file_path)
{
  return(read.table(file = bioneer_file_path,
                    header = TRUE,
                    sep = "\t",
                    stringsAsFactors = FALSE,
                    row.names = NULL))
}
