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

##with time varying confounder

data(casetimecontrols_tvc)

ctcfit_z <- CXO_tc_wt(data=casetimecontrols_tvc, exposure=ex, event=event, Id=Pt_ID, tvc=z)
test_that("Output is of correct format",{
  expect_length(ctcfit_z,23) #Returns something with a length of 23
  expect_no_error(ctcfit_z) #Returns no errors or warnings
  expect_type(ctcfit_z, "list")  #Returns a list

})


ctcfit_z.b <- CXO_tc_wt_boot(data=casetimecontrols_tvc, exposure=ex, event=event, Id=Pt_ID, B=3)

test_that("Output is of correct format",{
  expect_length(ctcfit_z.b,5) #Returns something with a length of 5
  expect_no_error(ctcfit_z.b) #Returns no errors or warnings
  expect_type(ctcfit_z.b, "list")  #Returns a list

})
