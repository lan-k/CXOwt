# test_that("Description of test",{
#   expect_(actual, expectation)
# })
data(cases)

test_that("Output is of correct format",{
  expect_length(mhor(Event ~ Id/ex, data=cases, digits=2),4) #Returns something with a length of 4
  expect_no_condition(mhor(Event ~ Id/ex, data=cases, digits=2)) #Returns no errors or warnings
})



test_that("Output is of correct format",{
  expect_length(SCL_bias(data=cases, exposure=ex, event=Event, Id=Id),3) #Returns something with a length of 4
  expect_no_error(SCL_bias(data=cases, exposure=ex, event=Event, Id=Id)) #Returns no errors or warnings

})

cfit <- CXO_wt(data=cases, exposure=ex, event=Event, Id=Id)
test_that("Output is of correct format",{
  expect_length(cfit,23) #Returns something with a length of 23
  expect_no_error(cfit) #Returns no errors or warnings
  expect_type(cfit, "list")  #Returns a list

})

cfit.b <- CXO_wt_boot(data=cases, exposure=ex, event=Event, Id = Id, B=3)

test_that("Output is of correct format",{
  expect_length(cfit.b,5) #Returns something with a length of 5
  expect_no_error(cfit.b) #Returns no errors or warnings
  expect_type(cfit.b, "list")  #Returns a list

})

##with time varying confounder

data(cases_tvc)

cfit_z <- CXO_wt(data=cases_tvc, exposure=ex, event=event, Id=Pt_ID, tvc=z)
test_that("Output is of correct format",{
  expect_length(cfit_z,23) #Returns something with a length of 23
  expect_no_error(cfit_z) #Returns no errors or warnings
  expect_type(cfit_z, "list")  #Returns a list

})


cfit_z.b <- CXO_wt_boot(data=cases_tvc, exposure=ex, event=event, Id=Pt_ID, B=3)

test_that("Output is of correct format",{
  expect_length(cfit_z.b,5) #Returns something with a length of 5
  expect_no_error(cfit_z.b) #Returns no errors or warnings
  expect_type(cfit_z.b, "list")  #Returns a list

})


