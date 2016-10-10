context('pinerrordetector')
test_that('1. convert_down_across',
          {
            expect_identical(a1[25:50, , drop = FALSE], a2)  #1 across to down
            expect_identical(a11, a12)                       #2 vector and data frame formats of data_from
            expect_identical(b1[25:50, , drop = FALSE], b2)  #3 down to across
            expect_identical(c1[25:50, , drop = FALSE], c2)  #4 across to across
            expect_identical(d1[25:50, , drop = FALSE], d2)  #5 No Plate Coordinates
          })

test_that('2. plate_coords',
          {
            expect_identical(e1[25:50, , drop = FALSE], e2)  #6 across
            expect_identical(f1[25:50, , drop = FALSE], f2)  #7 down
          })

test_that('3. convert_large_to_small',
          {
            expect_identical(g11[1:50, , drop = FALSE], g12)  #8 across to across with plate coordinates
            expect_identical(g21[1:50, , drop = FALSE], g22)  #9 across to across without plate coordinates
            expect_identical(g31[1:50, , drop = FALSE], g32)  #10 down to down with plate coordinates
            expect_identical(g41[1:50, , drop = FALSE], g42)  #11 down to down without plate coordinates
            expect_identical(g51, g11)                        #12 across to across and data_from as data frame
            expect_identical(g61, g31)                        #13 down to down and data_from as data frame
            expect_identical(g71[1:50, , drop = FALSE], g72)  #14 across to down with plate coordinates
            expect_identical(g81[1:50, , drop = FALSE], g82)  #15 down to across with plate coordinates
          })

test_that('4. convert_small_to_large',
          {
            expect_identical(h11[1:50,  , drop = FALSE], h12) #16 across to across with plate coordinates (vector: 1to4)
            expect_identical(h21[1:50,  , drop = FALSE], h12) #17 across to across with plate coordinates (data frame: 1to4)
            expect_identical(h31[1:50,  , drop = FALSE], h32) #18 across to across with plate coordinates (data frame: 4to4)
            expect_identical(h41[1:50,  , drop = FALSE], h42) #19 across to across without plate coordinates (vector: 1to4)
            expect_identical(h51[1:50,  , drop = FALSE], h42) #20 across to across without plate coordinates (data frame: 1to4)
            expect_identical(h61[1:50,  , drop = FALSE], h62) #21 across to across without plate coordinates (data frame: 4to4)
            expect_identical(h71[1:50,  , drop = FALSE], h72) #22 down to down with plate coordinates (vector: 1to4)
            expect_identical(h81[1:50,  , drop = FALSE], h72) #23 down to down with plate coordinates (data frame: 1to4)
            expect_identical(h91[1:50,  , drop = FALSE], h92) #24 down to down with plate coordinates (data frame: 4to4)
            expect_identical(h101[1:50,  , drop = FALSE], h102) #25 down to down without plate coordinates (vector: 1to4)
            expect_identical(h111[1:50,  , drop = FALSE], h102) #26 down to down without plate coordinates (data frame: 1to4)
            expect_identical(h121[1:50,  , drop = FALSE], h122) #27 down to down without plate coordinates (data frame: 4to4)
          })

test_that('5. convert_sl_ls_ad_da',
          {
            expect_identical(i11, i01)    #28 across to across
            expect_identical(i21, i02)    #29 down to down
            expect_identical(i31, i01)    #30 across to down
            expect_identical(i41, i02)    #31 down to across
          })

test_that('6. simulated_data_1536',
          {
            expect_identical(j11, j12)  #32 # simulated data using colonyarea data subtypes
          })

test_that('7. categorize_data',
          {
            expect_identical(which(k15 == 'Empty'), which(k12$y == 'Empty'))
            expect_identical(which(k15 == 'Pinning Error'), which(k12$y == 'Pinning Error'))
            expect_identical(which(k15 == 'Lessthan 25% Plate Median'), which(k12$y == 'Lessthan 25% Plate Median'))
            expect_identical(which(k15 == 'Lessthan Plate Median'), which(k12$y == 'Lessthan Plate Median'))
            expect_identical(which(k15 == 'Morethan 90% Plate Median'), which(k12$y == 'Morethan 90% Plate Median'))
            expect_identical(which(k15 == 'Morethan Plate Median'), which(k12$y == 'Morethan Plate Median'))
          })

test_that('8. plate_ncol',
          {
            expect_true(plate_ncol(96) == 12 &&
                          plate_ncol(384) == 24 &&
                          plate_ncol(1536) ==  48 &&
                          plate_ncol(6144) == 96)
          })

test_that('9. plate_nrow',
          {
            expect_true(plate_nrow(96) == 8 &&
                          plate_nrow(384) == 16 &&
                          plate_nrow(1536) ==  32 &&
                          plate_nrow(6144) == 64)
          })

test_that('10.plate_nrow',
          {
            expect_identical(plate_letters(), l11)
          })

test_that('11. plate_median',
          {
            expect_true( plate_median(c(2,3), 0:20) == 11)
          })

test_that('12. right_line_indices',
          {
            expect_identical(right_line_indices(96), as.numeric(90:95))
            expect_identical(right_line_indices(384), as.numeric(370:383))
            expect_identical(right_line_indices(1536), as.numeric(1506:1535))
            expect_identical(right_line_indices(6144), as.numeric(6082:6143))
          })

test_that('13. left_line_indices',
          {
            expect_identical(left_line_indices(96), as.numeric(2:7))
            expect_identical(left_line_indices(384), as.numeric(2:15))
            expect_identical(left_line_indices(1536), as.numeric(2:31))
            expect_identical(left_line_indices(6144), as.numeric(2:63))
          })

test_that('14. top_line_indices',
          {
            expect_identical(top_line_indices(96), m11)
            expect_identical(top_line_indices(384), m12)
            expect_identical(top_line_indices(1536), m13)
            expect_identical(top_line_indices(6144), m14)
          })

test_that('15. bottom_line_indices',
          {
            expect_identical(bottom_line_indices(96), n11)
            expect_identical(bottom_line_indices(384), n12)
            expect_identical(bottom_line_indices(1536), n13)
            expect_identical(bottom_line_indices(6144), n14)
          })

test_that('16. topleft_corner_neighbors',
          {
            expect_identical(topleft_corner_neighbors(96), as.numeric(c(2,9,10)))
            expect_identical(topleft_corner_neighbors(384), as.numeric(c(2,17,18)))
            expect_identical(topleft_corner_neighbors(1536), as.numeric(c(2,33,34)))
            expect_identical(topleft_corner_neighbors(6144), as.numeric(c(2,65,66)))
          })

test_that('17. topright_corner_neighbors',
          {
            expect_identical(topright_corner_neighbors(96), as.numeric(c(81,82,90)))
            expect_identical(topright_corner_neighbors(384), as.numeric(c(353,354,370)))
            expect_identical(topright_corner_neighbors(1536), as.numeric(c(1473,1474,1506)))
            expect_identical(topright_corner_neighbors(6144), as.numeric(c(6017,6018,6082)))
          })

test_that('18. bottomleft_corner_neighbors',
          {
            expect_identical(bottomleft_corner_neighbors(96), as.numeric(c(7,15,16)))
            expect_identical(bottomleft_corner_neighbors(384), as.numeric(c(15,31,32)))
            expect_identical(bottomleft_corner_neighbors(1536), as.numeric(c(31,63,64)))
            expect_identical(bottomleft_corner_neighbors(6144), as.numeric(c(63,127,128)))
          })

test_that('19. bottomright_corner_neighbors',
          {
            expect_identical(bottomright_corner_neighbors(96), as.numeric(c(87,88,95)))
            expect_identical(bottomright_corner_neighbors(384), as.numeric(c(367,368,383)))
            expect_identical(bottomright_corner_neighbors(1536), as.numeric(c(1503,1504,1535)))
            expect_identical(bottomright_corner_neighbors(6144), as.numeric(c(6079,6080,6143)))
          })

test_that('20. top_line_neighbors',
          {
            expect_identical(top_line_neighbors(96, 65), as.numeric(c(57,58,66,73,74)))
            expect_identical(top_line_neighbors(384, 65), as.numeric(c(49,50,66,81,82)))
            expect_identical(top_line_neighbors(1536, 65), as.numeric(c(33,34,66,97,98)))
            expect_identical(top_line_neighbors(6144, 65), as.numeric(c(1,2,66,129,130)))
          })

test_that('21. bottom_line_neighbors',
          {
            expect_identical(bottom_line_neighbors(96, 16), as.numeric(c(7,8,15,15,24)))
            expect_identical(bottom_line_neighbors(384, 32), as.numeric(c(15,16,31,31,48)))
            expect_identical(bottom_line_neighbors(1536, 64), as.numeric(c(31,32,63,63,96)))
            expect_identical(bottom_line_neighbors(6144, 128), as.numeric(c(63,64,127,127,192)))
          })

test_that('22. left_line_neighbors',
          {
            expect_identical(left_line_neighbors(96, 2), as.numeric(c(1,3,9,10,11)))
            expect_identical(left_line_neighbors(384, 2), as.numeric(c(1,3,17,18,19)))
            expect_identical(left_line_neighbors(1536, 2), as.numeric(c(1,3,33,34,35)))
            expect_identical(left_line_neighbors(6144, 2), as.numeric(c(1,3,65,66,67)))
          })

test_that('23. right_line_neighbors',
          {
            expect_identical(right_line_neighbors(96, 90), as.numeric(c(81,82,83,89,91)))
            expect_identical(right_line_neighbors(384, 370), as.numeric(c(353,354,355,369,371)))
            expect_identical(right_line_neighbors(1536, 1506), as.numeric(c(1473,1474,1475,1505,1507)))
            expect_identical(right_line_neighbors(6144, 6082), as.numeric(c(6017,6018,6019,6081,6083)))
          })

test_that('24. middle_neighbors',
          {
            expect_identical(middle_neighbors(96, 66), as.numeric(c(57,58,59,65,67,73,74,75)))
            expect_identical(middle_neighbors(384, 66), as.numeric(c(49,50,51,65,67,81,82,83)))
            expect_identical(middle_neighbors(1536, 66), as.numeric(c(33,34,35,65,67,97,98,99)))
            expect_identical(middle_neighbors(6144, 66), as.numeric(c(1,2,3,65,67,129,130,131)))
          })

test_that('25. not_middle_neighbors',
          {
            expect_identical(not_middle_neighbors(96, 1), as.numeric(c(2,9,10)))
            expect_identical(not_middle_neighbors(384, 1), as.numeric(c(2,17,18)))
            expect_identical(not_middle_neighbors(1536, 1), as.numeric(c(2,33,34)))
            expect_identical(not_middle_neighbors(6144, 1), as.numeric(c(2,65,66)))
          })

test_that('26. not_middle_indices',
          {
            expect_identical(not_middle_indices(96), o11)
            expect_identical(not_middle_indices(384), o12)
            expect_identical(not_middle_indices(1536), o13)
            expect_identical(not_middle_indices(6144), o14)
          })

test_that('27. indices_4_replicates',
          {
            expect_identical(indices_4_replicates(plate_from    = 384,
                                                  plate_to      = 96,
                                                  out_data_flow = 'across'), p11)
            expect_identical(indices_4_replicates(plate_from    = 384,
                                                  plate_to      = 96,
                                                  out_data_flow = 'down'), p12)
          })

test_that('28. parameter1',
          {
            expect_identical(parameter1(colony = 34,
                                        combinations = c(1,2,3,33,35,65),       # colony_area:  c(0,0,0,672,643,502)
                                        colony_area_raw_data = c(0,0,0,0,0,0,0,0,0,0,0,0,132,132,142,142,502,502,190,
                                                                 190,288,288,218,218,1002,1002,1052,1052,860,860,980,
                                                                 980,672,672,643,643,820,820,1158,1158,1153,1153,1154,
                                                                 1154,1089,1089,1195,1195,0,0,0,0,0,0,0,0,0,0,0,0,132,
                                                                 132,142,142,502,502),
                                        plate_median_threshold = 150,
                                        excluded_colonies = c(2),
                                        param1_threshold = 6), -1)
            expect_identical(parameter1(colony = 34,
                                        combinations = c(1,2,3,33,35,65),     # colony_area:  c(0,0,0,672,643,502)
                                        colony_area_raw_data = c(0,0,0,0,0,0,0,0,0,0,0,0,132,132,142,142,502,502,190,
                                                                 190,288,288,218,218,1002,1002,1052,1052,860,860,980,
                                                                 980,672,672,643,643,820,820,1158,1158,1153,1153,1154,
                                                                 1154,1089,1089,1195,1195,0,0,0,0,0,0,0,0,0,0,0,0,132,
                                                                 132,142,142,502,502),
                                        plate_median_threshold = 150,
                                        excluded_colonies = c(2),
                                        param1_threshold = 3), c(2,34))
            expect_identical(parameter1(colony = 34,
                                        combinations = c(1,2,3,33,35,65),     # colony_area:  c(0,0,0,672,643,502)
                                        colony_area_raw_data = c(0,0,0,0,0,0,0,0,0,0,0,0,132,132,142,142,502,502,190,
                                                                 190,288,288,218,218,1002,1002,1052,1052,860,860,980,
                                                                 980,672,672,643,643,820,820,1158,1158,1153,1153,1154,
                                                                 1154,1089,1089,1195,1195,0,0,0,0,0,0,0,0,0,0,0,0,132,
                                                                 132,142,142,502,502),
                                        plate_median_threshold = 672,
                                        excluded_colonies = c(2),
                                        param1_threshold = 6), c(2,34))
          })

test_that('29. parameter2',
          {
            expect_identical(parameter2(colony = 34,
                                        combinations = c(1,2,3,33,35,65),
                                        excluded_colonies = c(2),
                                        param2_threshold = 2), -1)

            expect_identical(parameter2(colony = 34,
                                        combinations = c(1,2,3,33,35,65),
                                        excluded_colonies = c(1, 2),
                                        param2_threshold = 2), c(1,2,34))
          })

test_that('30. variation1',
          {
            # param1: 6 | param2: 2 | excluded_colonies = NULL
            expect_identical(variation1(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = NULL,
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                          153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                          348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                          675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                          770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                          865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                          918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                        #                        'across',"across",FALSE)$y %in% 'Empty')
                                        param1_threshold = 6,
                                        param2_threshold = 2), NULL)

            # param1: 3 | param2: 2 | excluded_colonies = c(2)
            expect_identical(variation1(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 5L, 1L, 2L, 6L, 1L,
                                                                    2L, 7L, 1L, 2L, 8L, 1L, 3L, 4L, 1L, 3L, 5L, 1L, 3L, 6L, 1L, 3L,
                                                                    7L, 1L, 3L, 8L, 1L, 4L, 5L, 1L, 4L, 6L, 1L, 4L, 7L, 1L, 4L, 8L,
                                                                    1L, 5L, 6L, 1L, 5L, 7L, 1L, 5L, 8L, 1L, 6L, 7L, 1L, 6L, 8L, 1L,
                                                                    7L, 8L, 2L, 3L, 4L, 2L, 3L, 5L, 2L, 3L, 6L, 2L, 3L, 7L, 2L, 3L,
                                                                    8L, 2L, 4L, 5L, 2L, 4L, 6L, 2L, 4L, 7L, 2L, 4L, 8L, 2L, 5L, 6L,
                                                                    2L, 5L, 7L, 2L, 5L, 8L, 2L, 6L, 7L, 2L, 6L, 8L, 2L, 7L, 8L, 3L,
                                                                    4L, 5L, 3L, 4L, 6L, 3L, 4L, 7L, 3L, 4L, 8L, 3L, 5L, 6L, 3L, 5L,
                                                                    7L, 3L, 5L, 8L, 3L, 6L, 7L, 3L, 6L, 8L, 3L, 7L, 8L, 4L, 5L, 6L,
                                                                    4L, 5L, 7L, 4L, 5L, 8L, 4L, 6L, 7L, 4L, 6L, 8L, 4L, 7L, 8L, 5L,
                                                                    6L, 7L, 5L, 6L, 8L, 5L, 7L, 8L, 6L, 7L, 8L),
                                                                  .Dim = c(3L, 56L)), # combn(length(middle_neighbors(1536, 41)), 3)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                          153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                          348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                          675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                          770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                          865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                          918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                        #                        'across',"across",FALSE)$y %in% 'Empty')
                                        param1_threshold = 3,
                                        param2_threshold = 2), c(2, 41))


            # param1: 6 | param2: 1 | excluded_colonies = c(2,42,42)
            expect_identical(variation1(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2,42,42),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                          153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                          348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                          675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                          770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                          865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                          918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                        #                        'across',"across",FALSE)$y %in% 'Empty')
                                        param1_threshold = 6,
                                        param2_threshold = 1), c(2,42,41))


            })

test_that('31. variation2',
          {
            # param1: 6 | param2: 2 | excluded_colonies = NULL
            expect_identical(variation2(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = NULL,
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        param1_threshold = 6,
                                        param2_threshold = 2), 41)

            # param1: 3 | param2: 1 | excluded_colonies = c(2)
            expect_identical(variation2(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 5L, 1L, 2L, 6L, 1L,
                                                                    2L, 7L, 1L, 2L, 8L, 1L, 3L, 4L, 1L, 3L, 5L, 1L, 3L, 6L, 1L, 3L,
                                                                    7L, 1L, 3L, 8L, 1L, 4L, 5L, 1L, 4L, 6L, 1L, 4L, 7L, 1L, 4L, 8L,
                                                                    1L, 5L, 6L, 1L, 5L, 7L, 1L, 5L, 8L, 1L, 6L, 7L, 1L, 6L, 8L, 1L,
                                                                    7L, 8L, 2L, 3L, 4L, 2L, 3L, 5L, 2L, 3L, 6L, 2L, 3L, 7L, 2L, 3L,
                                                                    8L, 2L, 4L, 5L, 2L, 4L, 6L, 2L, 4L, 7L, 2L, 4L, 8L, 2L, 5L, 6L,
                                                                    2L, 5L, 7L, 2L, 5L, 8L, 2L, 6L, 7L, 2L, 6L, 8L, 2L, 7L, 8L, 3L,
                                                                    4L, 5L, 3L, 4L, 6L, 3L, 4L, 7L, 3L, 4L, 8L, 3L, 5L, 6L, 3L, 5L,
                                                                    7L, 3L, 5L, 8L, 3L, 6L, 7L, 3L, 6L, 8L, 3L, 7L, 8L, 4L, 5L, 6L,
                                                                    4L, 5L, 7L, 4L, 5L, 8L, 4L, 6L, 7L, 4L, 6L, 8L, 4L, 7L, 8L, 5L,
                                                                    6L, 7L, 5L, 6L, 8L, 5L, 7L, 8L, 6L, 7L, 8L),
                                                                  .Dim = c(3L, 56L)), # combn(length(middle_neighbors(1536, 41)), 3)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        param1_threshold = 3,
                                        param2_threshold = 2), c(2, 41))


            # param1: 6 | param2: 1 | excluded_colonies = c(2,42,42)
            expect_identical(variation2(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2,42,42),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        param1_threshold = 6,
                                        param2_threshold = 1), c(2,42,41))


          })

test_that('32. variation3',
          {
            # param1: 6 | excluded_colonies = NULL
            expect_identical(variation3(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = NULL,
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                          153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                          348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                          675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                          770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                          865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                          918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                        #                        'across',"across",FALSE)$y %in% 'Empty')
                                        param1_threshold = 6), NULL)

            # param1: 3 | excluded_colonies = c(2)
            expect_identical(variation3(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 5L, 1L, 2L, 6L, 1L,
                                                                    2L, 7L, 1L, 2L, 8L, 1L, 3L, 4L, 1L, 3L, 5L, 1L, 3L, 6L, 1L, 3L,
                                                                    7L, 1L, 3L, 8L, 1L, 4L, 5L, 1L, 4L, 6L, 1L, 4L, 7L, 1L, 4L, 8L,
                                                                    1L, 5L, 6L, 1L, 5L, 7L, 1L, 5L, 8L, 1L, 6L, 7L, 1L, 6L, 8L, 1L,
                                                                    7L, 8L, 2L, 3L, 4L, 2L, 3L, 5L, 2L, 3L, 6L, 2L, 3L, 7L, 2L, 3L,
                                                                    8L, 2L, 4L, 5L, 2L, 4L, 6L, 2L, 4L, 7L, 2L, 4L, 8L, 2L, 5L, 6L,
                                                                    2L, 5L, 7L, 2L, 5L, 8L, 2L, 6L, 7L, 2L, 6L, 8L, 2L, 7L, 8L, 3L,
                                                                    4L, 5L, 3L, 4L, 6L, 3L, 4L, 7L, 3L, 4L, 8L, 3L, 5L, 6L, 3L, 5L,
                                                                    7L, 3L, 5L, 8L, 3L, 6L, 7L, 3L, 6L, 8L, 3L, 7L, 8L, 4L, 5L, 6L,
                                                                    4L, 5L, 7L, 4L, 5L, 8L, 4L, 6L, 7L, 4L, 6L, 8L, 4L, 7L, 8L, 5L,
                                                                    6L, 7L, 5L, 6L, 8L, 5L, 7L, 8L, 6L, 7L, 8L),
                                                                  .Dim = c(3L, 56L)), # combn(length(middle_neighbors(1536, 41)), 3)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                          153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                          348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                          675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                          770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                          865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                          918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                        #                        'across',"across",FALSE)$y %in% 'Empty')
                                        param1_threshold = 3), c(2, 41))

          })

test_that('33. variation4',
          {
            # param1: 6 | excluded_colonies = NULL
            expect_identical(variation4(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 7L, 1L,
                                                                    2L, 3L, 4L, 5L, 8L, 1L, 2L, 3L, 4L, 6L, 7L, 1L, 2L, 3L, 4L, 6L,
                                                                    8L, 1L, 2L, 3L, 4L, 7L, 8L, 1L, 2L, 3L, 5L, 6L, 7L, 1L, 2L, 3L,
                                                                    5L, 6L, 8L, 1L, 2L, 3L, 5L, 7L, 8L, 1L, 2L, 3L, 6L, 7L, 8L, 1L,
                                                                    2L, 4L, 5L, 6L, 7L, 1L, 2L, 4L, 5L, 6L, 8L, 1L, 2L, 4L, 5L, 7L,
                                                                    8L, 1L, 2L, 4L, 6L, 7L, 8L, 1L, 2L, 5L, 6L, 7L, 8L, 1L, 3L, 4L,
                                                                    5L, 6L, 7L, 1L, 3L, 4L, 5L, 6L, 8L, 1L, 3L, 4L, 5L, 7L, 8L, 1L,
                                                                    3L, 4L, 6L, 7L, 8L, 1L, 3L, 5L, 6L, 7L, 8L, 1L, 4L, 5L, 6L, 7L,
                                                                    8L, 2L, 3L, 4L, 5L, 6L, 7L, 2L, 3L, 4L, 5L, 6L, 8L, 2L, 3L, 4L,
                                                                    5L, 7L, 8L, 2L, 3L, 4L, 6L, 7L, 8L, 2L, 3L, 5L, 6L, 7L, 8L, 2L,
                                                                    4L, 5L, 6L, 7L, 8L, 3L, 4L, 5L, 6L, 7L, 8L),
                                                                  .Dim = c(6L, 28L)), # combn(length(middle_neighbors(1536, 41)), 6)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = NULL,
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        param1_threshold = 6), 41)

            # param1: 3 | excluded_colonies = c(2)
            expect_identical(variation4(colony = 41,
                                        combin_coords = structure(c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 5L, 1L, 2L, 6L, 1L,
                                                                    2L, 7L, 1L, 2L, 8L, 1L, 3L, 4L, 1L, 3L, 5L, 1L, 3L, 6L, 1L, 3L,
                                                                    7L, 1L, 3L, 8L, 1L, 4L, 5L, 1L, 4L, 6L, 1L, 4L, 7L, 1L, 4L, 8L,
                                                                    1L, 5L, 6L, 1L, 5L, 7L, 1L, 5L, 8L, 1L, 6L, 7L, 1L, 6L, 8L, 1L,
                                                                    7L, 8L, 2L, 3L, 4L, 2L, 3L, 5L, 2L, 3L, 6L, 2L, 3L, 7L, 2L, 3L,
                                                                    8L, 2L, 4L, 5L, 2L, 4L, 6L, 2L, 4L, 7L, 2L, 4L, 8L, 2L, 5L, 6L,
                                                                    2L, 5L, 7L, 2L, 5L, 8L, 2L, 6L, 7L, 2L, 6L, 8L, 2L, 7L, 8L, 3L,
                                                                    4L, 5L, 3L, 4L, 6L, 3L, 4L, 7L, 3L, 4L, 8L, 3L, 5L, 6L, 3L, 5L,
                                                                    7L, 3L, 5L, 8L, 3L, 6L, 7L, 3L, 6L, 8L, 3L, 7L, 8L, 4L, 5L, 6L,
                                                                    4L, 5L, 7L, 4L, 5L, 8L, 4L, 6L, 7L, 4L, 6L, 8L, 4L, 7L, 8L, 5L,
                                                                    6L, 7L, 5L, 6L, 8L, 5L, 7L, 8L, 6L, 7L, 8L),
                                                                  .Dim = c(3L, 56L)), # combn(length(middle_neighbors(1536, 41)), 3)
                                        neighbors_selected_colony = c(8, 9, 10, 40, 42, 72, 73, 74),  # middle_neighbors(1536, 41)
                                        excluded_colonies = c(2),
                                        colony_area_raw_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 132, 132, 142, 142, 502,
                                                                 502, 190, 190, 288, 288, 218, 218, 1002, 1002, 1052, 1052, 860,
                                                                 860, 980, 980, 672, 672, 643, 643, 820, 820, 1158, 1158, 1153,
                                                                 1153, 1154, 1154, 1089, 1089, 1195, 1195, 0, 0, 0, 0, 0, 0, 0,
                                                                 0, 0, 0, 0, 0, 132, 132, 142, 142, 502, 502, 190, 190, 288, 288,
                                                                 218, 218, 1002, 1002),  # data_area$y[1:74]
                                        plate_median_threshold = 1100,
                                        param1_threshold = 3), c(2, 41))
          })

test_that('34. excluded_coloniesA',
          {
            # middle colonies
            expect_identical(excluded_coloniesA(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                param2_threshold = 2,
                                                is_middle = TRUE),
                             c(499L, 500L, 531L, 532L, 721L, 770L, 813L, 814L, 817L, 818L,
                               821L, 862L, 913L, 1264L, 1265L, 1266L, 1267L, 1268L, 1269L, 1270L,
                               1271L, 1272L, 1273L, 1274L, 1275L, 1276L, 1277L, 1278L, 1297L,
                               1298L, 1299L, 1300L, 1301L, 1302L, 1303L, 1304L, 1305L, 1306L,
                               1307L, 1308L, 1309L, 1310L, 1311L))

            # not middle colonies
            expect_identical(excluded_coloniesA(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                param2_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 14, 15, 16, 64, 160, 352, 736, 769, 832,
                               865, 928, 1312))
          })

test_that('35. excluded_coloniesB',
          {
            # middle colonies
            expect_identical(excluded_coloniesB(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                param2_threshold = 2,
                                                is_middle = TRUE),
                             c(491L, 492L, 499L, 500L, 507L, 508L, 523L, 524L, 531L, 532L,
                               539L, 540L, 555L, 556L, 571L, 572L, 688L, 689L, 690L, 721L, 722L,
                               723L, 738L, 770L, 771L, 781L, 782L, 783L, 784L, 785L, 786L, 789L,
                               790L, 813L, 814L, 817L, 818L, 819L, 820L, 821L, 822L, 829L, 830L,
                               831L, 834L, 837L, 838L, 844L, 861L, 862L, 866L, 867L, 868L, 869L,
                               870L, 880L, 881L, 882L, 913L, 914L, 915L, 1264L, 1265L, 1266L,
                               1267L, 1268L, 1269L, 1270L, 1271L, 1272L, 1273L, 1274L, 1275L,
                               1276L, 1277L, 1278L, 1297L, 1298L, 1299L, 1300L, 1301L, 1302L,
                               1303L, 1304L, 1305L, 1306L, 1307L, 1308L, 1309L, 1310L, 1311L))

            # not middle colonies
            expect_identical(excluded_coloniesB(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                param2_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 64, 160,
                               352, 736, 737, 769, 832, 833, 865, 928, 1312))
          })

test_that('36. excluded_coloniesC',
          {
            # middle colonies
            expect_identical(excluded_coloniesC(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                is_middle = TRUE),
                             c(499L, 500L, 531L, 532L, 721L, 770L, 813L, 814L, 817L, 818L,
                               821L, 862L, 913L, 1264L, 1265L, 1266L, 1267L, 1268L, 1269L, 1270L,
                               1271L, 1272L, 1273L, 1274L, 1275L, 1276L, 1277L, 1278L, 1297L,
                               1298L, 1299L, 1300L, 1301L, 1302L, 1303L, 1304L, 1305L, 1306L,
                               1307L, 1308L, 1309L, 1310L, 1311L))
            # not middle colonies
            expect_identical(excluded_coloniesC(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 14, 15, 16, 64, 160, 352, 736, 769, 832,
                               865, 928, 1312))
          })

test_that('37. excluded_coloniesD',
          {

            # middle colonies
            expect_identical(excluded_coloniesD(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                is_middle = TRUE),
                             c(491L, 492L, 499L, 500L, 507L, 508L, 523L, 524L, 531L, 532L,
                               539L, 540L, 555L, 556L, 571L, 572L, 688L, 689L, 690L, 721L, 722L,
                               723L, 738L, 770L, 771L, 781L, 782L, 783L, 784L, 785L, 786L, 789L,
                               790L, 813L, 814L, 817L, 818L, 819L, 820L, 821L, 822L, 829L, 830L,
                               831L, 834L, 837L, 838L, 844L, 861L, 862L, 866L, 867L, 868L, 869L,
                               870L, 880L, 881L, 882L, 913L, 914L, 915L, 1264L, 1265L, 1266L,
                               1267L, 1268L, 1269L, 1270L, 1271L, 1272L, 1273L, 1274L, 1275L,
                               1276L, 1277L, 1278L, 1297L, 1298L, 1299L, 1300L, 1301L, 1302L,
                               1303L, 1304L, 1305L, 1306L, 1307L, 1308L, 1309L, 1310L, 1311L))
            # not middle colonies
            expect_identical(excluded_coloniesD(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 64, 160,
                               352, 736, 737, 769, 832, 833, 865, 928, 1312))
          })

test_that('38. excluded_coloniesE',
          {

            # middle colonies
            expect_identical(excluded_coloniesE(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                param2_threshold = 2,
                                                is_middle = TRUE),
                             c(499L, 500L, 531L, 532L, 1264L, 1265L, 1266L, 1267L, 1268L,
                               1269L, 1270L, 1271L, 1272L, 1273L, 1274L, 1275L, 1276L, 1277L,
                               1278L, 1297L, 1298L, 1299L, 1300L, 1301L, 1302L, 1303L, 1304L,
                               1305L, 1306L, 1307L, 1308L, 1309L, 1310L, 1311L))

            # not middle colonies
            expect_identical(excluded_coloniesE(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                param2_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 14, 15, 16, 64, 160, 352, 736, 832, 928,
                               1312))
          })

test_that('39. excluded_coloniesF',
          {

            # middle colonies
            expect_identical(excluded_coloniesF(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                param2_threshold = 2,
                                                is_middle = TRUE),
                             c(499L, 500L, 523L, 524L, 531L, 532L, 571L, 572L, 688L, 689L,
                               690L, 738L, 771L, 781L, 782L, 783L, 784L, 785L, 786L, 789L, 790L,
                               819L, 820L, 829L, 830L, 831L, 834L, 837L, 838L, 844L, 880L, 881L,
                               882L, 1264L, 1265L, 1266L, 1267L, 1268L, 1269L, 1270L, 1271L,
                               1272L, 1273L, 1274L, 1275L, 1276L, 1277L, 1278L, 1297L, 1298L,
                               1299L, 1300L, 1301L, 1302L, 1303L, 1304L, 1305L, 1306L, 1307L,
                               1308L, 1309L, 1310L, 1311L))
            # not middle colonies
            expect_identical(excluded_coloniesF(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                param2_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 8, 13, 14, 15, 16, 64, 160, 352, 736, 737,
                               832, 833, 928, 1312))
          })

test_that('40. excluded_coloniesG',
          {

            # middle colonies
            expect_identical(excluded_coloniesG(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                is_middle = TRUE),
                             c(499L, 500L, 531L, 532L, 1264L, 1265L, 1266L, 1267L, 1268L,
                               1269L, 1270L, 1271L, 1272L, 1273L, 1274L, 1275L, 1276L, 1277L,
                               1278L, 1297L, 1298L, 1299L, 1300L, 1301L, 1302L, 1303L, 1304L,
                               1305L, 1306L, 1307L, 1308L, 1309L, 1310L, 1311L))
            # not middle colonies
            expect_identical(excluded_coloniesG(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 14, 15, 16, 64, 160, 352, 736, 832, 928,
                               1312))
          })

test_that('41. excluded_coloniesH',
          {

            # middle colonies
            expect_identical(excluded_coloniesH(plateformat = 1536,
                                                colony_indices = (1:1536)[-(not_middle_indices(1536))],
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 3,
                                                is_middle = TRUE),
                             c(499L, 500L, 523L, 524L, 531L, 532L, 571L, 572L, 688L, 689L,
                               690L, 738L, 771L, 781L, 782L, 783L, 784L, 785L, 786L, 789L, 790L,
                               819L, 820L, 829L, 830L, 831L, 834L, 837L, 838L, 844L, 880L, 881L,
                               882L, 1264L, 1265L, 1266L, 1267L, 1268L, 1269L, 1270L, 1271L,
                               1272L, 1273L, 1274L, 1275L, 1276L, 1277L, 1278L, 1297L, 1298L,
                               1299L, 1300L, 1301L, 1302L, 1303L, 1304L, 1305L, 1306L, 1307L,
                               1308L, 1309L, 1310L, 1311L))
            # not middle colonies
            expect_identical(excluded_coloniesH(plateformat = 1536,
                                                colony_indices = not_middle_indices(1536),
                                                colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                           in_data_flow = "across",
                                                                                           out_data_flow = "across",
                                                                                           is_plate_coords = TRUE)$y,
                                                empty_indices = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                  153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                  348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                  675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                  770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                  865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                  918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                #                        'across',"across",FALSE)$y %in% 'Empty'),,
                                                excluded_colonies = c(),
                                                plate_median_threshold = 150,
                                                param1_threshold = 2,
                                                is_middle = FALSE),
                             c(2, 3, 4, 5, 6, 7, 8, 13, 14, 15, 16, 64, 160, 352, 736, 737,
                               832, 833, 928, 1312))
          })

test_that('42. neighborful_algorithm',
          {
            expect_identical(neighborful_algorithm(plateformat           = 1536,
                                                   colony_area_raw_data  = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                               in_data_flow = "across",
                                                                                               out_data_flow = "across",
                                                                                               is_plate_coords = TRUE)$y,
                                                   empty_indices         = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                             153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                             348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                             675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                             770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                             865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                             918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                   #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                   excluded_colonies     = c(),
                                                   percent_median_thresh = 25,
                                                   param1_thresh         = 3,
                                                   param2_thresh         = 2,
                                                   is_save               = FALSE,
                                                   excluded_file         = NULL),
                             q11)
          })

test_that('43. neighborless_algorithm',
          {
            expect_identical(neighborless_algorithm(plateformat          = 1536,
                                                    colony_area_raw_data = simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                                                                               in_data_flow = "across",
                                                                                               out_data_flow = "across",
                                                                                               is_plate_coords = TRUE)$y,
                                                    empty_indices        = c(9L, 10L, 11L, 12L, 57L, 58L, 59L, 60L, 105L, 106L, 107L, 108L,
                                                                             153L, 154L, 155L, 156L, 297L, 298L, 299L, 300L, 345L, 346L, 347L,
                                                                             348L, 491L, 492L, 507L, 508L, 539L, 540L, 555L, 556L, 673L, 674L,
                                                                             675L, 676L, 677L, 678L, 721L, 722L, 723L, 724L, 725L, 726L, 769L,
                                                                             770L, 773L, 774L, 813L, 814L, 817L, 818L, 821L, 822L, 861L, 862L,
                                                                             865L, 866L, 867L, 868L, 869L, 870L, 913L, 914L, 915L, 916L, 917L,
                                                                             918L), # which(convert_small_to_large(384,1536,colonyarea$data_subtypes,
                                                    #                        'across',"across",FALSE)$y %in% 'Empty'),
                                                    excluded_colonies    = c(),
                                                    is_save              = FALSE,
                                                    excluded_file        = NULL),
                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 49L, 50L, 51L, 52L, 53L, 54L,
                               55L, 56L, 97L, 98L, 99L, 100L, 101L, 102L, 103L, 104L, 145L,
                               146L, 147L, 148L, 149L, 150L, 151L, 152L, 289L, 290L, 291L, 292L,
                               293L, 294L, 295L, 296L, 337L, 338L, 339L, 340L, 341L, 342L, 343L,
                               344L, 483L, 484L, 531L, 532L, 771L, 772L, 777L, 778L, 783L, 784L,
                               789L, 790L, 795L, 796L, 819L, 820L, 825L, 826L, 831L, 832L, 837L,
                               838L, 843L, 844L, 1249L, 1250L, 1251L, 1252L, 1253L, 1254L, 1255L,
                               1256L, 1257L, 1258L, 1259L, 1260L, 1261L, 1262L, 1263L, 1264L,
                               1265L, 1266L, 1267L, 1268L, 1269L, 1270L, 1271L, 1272L, 1273L,
                               1274L, 1275L, 1276L, 1277L, 1278L, 1297L, 1298L, 1299L, 1300L,
                               1301L, 1302L, 1303L, 1304L, 1305L, 1306L, 1307L, 1308L, 1309L,
                               1310L, 1311L, 1312L, 1313L, 1314L, 1315L, 1316L, 1317L, 1318L,
                               1319L, 1320L, 1321L, 1322L, 1323L, 1324L, 1325L, 1326L))
          })
