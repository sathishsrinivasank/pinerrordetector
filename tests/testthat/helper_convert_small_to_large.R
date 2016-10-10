#1
# across to across with plate coordinates (vector: 1to4)
h11 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = 1:384,
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = TRUE)

# across to across with plate coordinates (data frame: 1to4)
h21 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = data.frame(y = 1:384),
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = TRUE)

h12 <- structure(list(row = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B"),
                      column = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
                                 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L,
                                 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L,
                                 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 1L, 2L),
                      y = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L,
                            9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L,
                            15L, 16L, 16L, 17L, 17L, 18L, 18L, 19L, 19L, 20L, 20L, 21L, 21L,
                            22L, 22L, 23L, 23L, 24L, 24L, 1L, 1L)),
                 .Names = c("row", "column", "y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")

# across to across with plate coordinates (data frame: 4to4)
h31 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = g21,
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = TRUE)

h32 <- structure(list(row = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                              "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B"),
                      column = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
                                 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L,
                                 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L,
                                 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 1L, 2L),
                      y = 1:50),
                 .Names = c("row", "column", "y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")

#2
# across to across without plate coordinates (vector: 1to4)
h41 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = 1:384,
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = FALSE)

# across to across without plate coordinates (data frame: 1to4)
h51 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = data.frame(y = 1:384),
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = FALSE)

h42 <- structure(list(y = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L,
                            9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L,
                            15L, 16L, 16L, 17L, 17L, 18L, 18L, 19L, 19L, 20L, 20L, 21L, 21L,
                            22L, 22L, 23L, 23L, 24L, 24L, 1L, 1L)),
                 .Names = c("y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")

# across to across without plate coordinates (data frame: 4to4)
h61 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = g21,          # across
                              in_data_flow = 'across',
                              out_data_flow = 'across',
                              is_plate_coords = FALSE)

h62 <- structure(list(y = 1:50),
                 .Names = c("y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")


#3
# down to down with plate coordinates (vector: 1to4)
h71 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = 1:384,
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = TRUE)

# down to down with plate coordinates (data frame: 1to4)
h81 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = data.frame(y = 1:384),
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = TRUE)

h72 <- structure(list(row = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
                              "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X",
                              "Y", "Z", "AA", "AB", "AC", "AD", "AE", "AF", "A", "B", "C",
                              "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                              "P", "Q", "R"),
                      column = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                      y = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L,
                            8L, 9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L,
                            15L, 15L, 16L, 16L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L,
                            6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L)),
                 .Names = c("row", "column", "y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")

# down to down with plate coordinates (data frame: 4to4)
h91 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = g21,
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = TRUE)

h92 <- structure(list(row = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
                              "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X",
                              "Y", "Z", "AA", "AB", "AC", "AD", "AE", "AF", "A", "B", "C",
                              "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                              "P", "Q", "R"),
                      column = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                      y = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
                            15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L,
                            27L, 28L, 29L, 30L, 31L, 32L, 49L, 50L, 51L, 52L, 53L, 54L,
                            55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L)),
                 .Names = c("row", "column", "y"),
                 row.names = c(NA, 50L),
                 class = "data.frame")

# down to down without plate coordinates (vector: 1to4)
h101 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = 1:384,
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = FALSE)

# down to down without plate coordinates (data frame: 1to4)
h111 <- convert_small_to_large(plate_from = 384,
                               plate_to = 1536,
                               data_from = data.frame(y = 1:384),
                               in_data_flow = 'down',
                               out_data_flow = 'down',
                               is_plate_coords = FALSE)

h102 <- structure(list(y = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L,
                             8L, 9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L,
                             15L, 15L, 16L, 16L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L,
                             6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L)),
                  .Names = c("y"),
                  row.names = c(NA, 50L),
                  class = "data.frame")


# down to down without plate coordinates (data frame: 4to4)
h121 <- convert_small_to_large(plate_from = 384,
                              plate_to = 1536,
                              data_from = g21,          # down
                              in_data_flow = 'down',
                              out_data_flow = 'down',
                              is_plate_coords = FALSE)

h122 <- structure(list(y = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
                             15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L,
                             27L, 28L, 29L, 30L, 31L, 32L, 49L, 50L, 51L, 52L, 53L, 54L,
                             55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L)),
                  .Names = c("y"),
                  row.names = c(NA, 50L),
                  class = "data.frame")


