# convert_data across -- down
plateformat = 384
n_col = plate_ncol(plateformat)
n_row = plate_nrow(plateformat)

# down to across
mydata_1 <- data.frame(stringsAsFactors = FALSE)
for(i in (n_row-1):0){
  for(j in 1:n_col){
    mydata_1 <- do.call('rbind', list(mydata_1, (colonyarea[((j*n_row)-i),])))
  }
}

# across to down
mydata_2 <- data.frame(stringsAsFactors = FALSE)
for(i in (n_col-1):0){
  for(j in 1:n_row){
    mydata_2 <- do.call('rbind', list(mydata_2, (mydata_1[((j*n_col)-i),])))
  }
}
