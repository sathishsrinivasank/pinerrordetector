comp_2_df <- function(data_1, data_2){
  data_1_rows <- nrow(data_1)
  data_2_rows <- nrow(data_2)

  if(data_1_rows != data_2_rows){
    stop('The given data must have same dimensions')
  }

  col1_uniq_1 <- unique(data_1[,1])
  col2_uniq_1 <- unique(data_1[,2])
  col1_uniq_2 <- unique(data_2[,1])
  col2_uniq_2 <- unique(data_2[,2])

  if(!all.equal(col1_uniq_1, col1_uniq_2) && !all.equal(col2_uniq_1, col2_uniq_2)){
    stop('The unique values of the first two columns must be same between two data')
  }

  result <- data.frame(matrix(NA, ncol = 3, nrow = data_1_rows),
                       stringsAsFactors = FALSE)

  names(result) <- c('row', 'column', 'checked' )

  result_counter <- 1

  for(i in col1_uniq_1){
    data_letters_1 <- data_1[which(data_1[,1] == i),]
    data_letters_2 <- data_2[which(data_2[,1] == i),]

    for(j in col2_uniq_1){
      data_1_val <- data_letters_1[which(data_letters_1[,2] == j), 3]

      data_2_val <- data_letters_2[which(data_letters_2[,2] == j), 3]

      if(data_1_val == data_2_val){
        result[result_counter, 1:3] <- c(i, j, 'pass')
      } else {
        result[result_counter, 1:3] <- c(i, j, 'fail')
      }

      result_counter <- result_counter + 1
    }
  }

  return(result)
}
