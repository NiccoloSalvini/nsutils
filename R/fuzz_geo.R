


fuzzy_geo_inner_join <- function(df1, df2, by, time_buffer) {

  fuzzyjoin::difference_inner_join(
    df1, df2,
    by,
    max_dist = time_buffer
  )


}

fuzzy_geo_left_join <- function(variables) {
  fuzzyjoin::difference_left_join(

  )

}


fuzzy_geo_right_join <- function(variables) {
  fuzzyjoin::difference_right_join(

  )

}



fuzzy_geo_semi_join <- function(variables) {
  fuzzyjoin::difference_semi_join(

  )

}


fuzzy_geo_anti_join <- function(variables) {
  fuzzyjoin::difference_anti_join(

  )

}




fuzzy_geo_full_join <- function(variables) {
  fuzzyjoin::difference_full_join(

  )

}



