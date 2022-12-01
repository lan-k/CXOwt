data(casetimecontrols)

ctcfit <- CXO_tc_wt(data=casetimecontrols,exposure=ex,event=Event,Id=Id)
test_that("Output is of correct format",{
  expect_length(ctcfit,23) #Returns something with a length of 23
  expect_no_error(ctcfit) #Returns no errors or warnings
  expect_type(ctcfit, "list")  #Returns a list

})

ctcfit.b <-CXO_tc_wt_boot(casetimecontrols, exposure = ex, event = Event, Id=Id, B = 3)

test_that("Output is of correct format",{
  expect_length(ctcfit.b,5) #Returns something with a length of 5
  expect_no_error(ctcfit.b) #Returns no errors or warnings
  expect_type(ctcfit.b, "list")  #Returns a list

})
