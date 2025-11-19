skip_on_cran()
context("align_move")

#if("align_move" %in% which_tests){
test_that("align_move (default)", {
  # correct calls
  x <- expect_is(align_move(m, verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, verbose = F, res = "max"), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, res = "mean", verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(m, res = units::set_units(4, "min"), verbose = F), "move2")
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  x <- expect_is(align_move(
    m, res = units::set_units(4, "min"), start_end_time = round.POSIXt(range(mt_time(m)), units = "hours"),
    verbose = F
  ), "move2")
  expect_equal(nrow(x), 458)
  expect_length(na.omit(unique(unlist(move2::mt_time_lags(x, units = "secs")))), 1)
  
  # false calls
  expect_error(align_move(NA, verbose = F)) # wrong class
  
  # warnings
  expect_warning(align_move(m, digit = "max", verbose = F))
})
#}

test_that("informative errors on bad temporal resolution inputs", {
  expect_error(align_move(m, res = FALSE, verbose = F))
  expect_error(align_move(m, start_end_time = T, verbose = F))
  expect_error(expect_warning(align_move(m, res = 1, unit = "days", verbose = F)))
  
  expect_error(
    align_move(m, res = units::set_units(20, "hours"), verbose = FALSE),
    "exceeds the temporal range used for alignment"
  )
  expect_error(
    align_move(
      m, 
      res = units::set_units(5, "hours"), 
      start_end_time = c(
        as.POSIXct("2018-05-15 08:00:00", "UTC"), 
        as.POSIXct("2018-05-15 12:00:00", "UTC")
      ),
      verbose = FALSE
    ),
    "exceeds the temporal range used for alignment"
  )
  expect_error(
    align_move(m, res = units::set_units(8, "hours"), verbose = FALSE),
    "too coarse for some tracks: \"T246a\", \"T342g\", \"T932u\""
  )
  expect_error(
    align_move(m, res = units::set_units(6, "hours"), verbose = FALSE),
    "too coarse for some tracks: \"T342g\", \"T932u\""
  )
  expect_warning(
    align_move(m, res = units::set_units(5, "hours"), verbose = FALSE),
    "only two positions for at least one track"
  )
})
