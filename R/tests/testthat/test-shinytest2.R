library(shinytest2)

# Happy path data
dir_data_happyPath = "../../../data/data_original.xlsx"

# Improperly formatted data - for error handling
dir_data_wrong_filetype   = "../../../data/data_incorrect_file_type.png"
dir_data_empty_column     = "../../../data/data_empty.xlsx"
dir_data_incorrect_format = "../../../data/data_incorrect_format.xlsx"
dir_data_corrupted        = "../../../data/data_corrupted.csv"

# Happy path -- intended usage
test_that("{shinytest2} recording: standard_usage", {
  app <- AppDriver$new(name = "standard_usage", height = 686, width = 1235)
  app$expect_values()
  app$upload_file(dataset = dir_data_happyPath)
  
  app$set_inputs(selectXY = "Nadir CD4þ T-cell count (cells/ml)")
  app$set_inputs(selectXY = c("Nadir CD4þ T-cell count (cells/ml)", "Plasma viremia (HIV-1 RNA copies/ml)"))
  app$set_inputs(selectZCont = "Binned CD4")
  app$set_inputs(selectZCont = character(0))
  app$set_inputs(selectZCont = "Inducible virus release (copies/mL culture supernatant) Resting CD4+T-cells")
  app$set_inputs(selectZCat = "Binned CD4")
  
  app$click("update") # Execute calculation
  app$wait_for_value(output = "downloadUI", timeout = 5000) # Wait for the download button to appear
  app$expect_download("download") # Expect the download
  app$click("clear") # Reset app
  
})


# Error path -- wrong file format
test_that("{shinytest2} recording: incorrect_filetype", {
  app <- AppDriver$new(name = "incorrect_filetype", height = 738, width = 1235)
  app$expect_values()
  
  app$upload_file(dataset = dir_data_wrong_filetype)
  app$click("update") # Execute calculation
  app$click("clear") # Reset app
})

# Error path -- column has entirely missing values
test_that("{shinytest2} recording: null_column", {
  app <- AppDriver$new(name = "null_column", height = 738, width = 1235)
  app$expect_values()
  app$upload_file(dataset = dir_data_empty_column)
  app$set_inputs(selectXY = "x")
  app$set_inputs(selectXY = c("x", "y"))
  app$set_inputs(selectZCont = "z")
  app$click("update")
  app$wait_for_value(output = "downloadUI", timeout = 5000) # Wait for the download button to appear
  app$expect_download("download")
  app$click("clear")
})

# Error path -- non-null cells outside of data range
test_that("{shinytest2} recording: misformatted_data", {
  app <- AppDriver$new(name = "misformatted_data", height = 738, width = 1235)
  app$expect_values()
  app$upload_file(dataset = dir_data_incorrect_format)
  app$set_inputs(selectXY = "column a")
  app$set_inputs(selectXY = c("column a", "...2"))
  app$set_inputs(selectZCont = "column c")
  app$set_inputs(selectZCat = "column b")
  app$click("update")
  app$wait_for_value(output = "downloadUI", timeout = 5000) # Wait for the download button to appear
  app$expect_download("download")
  app$click("clear")
})

# Error path -- corrupted data (image file saved as .CSV)
test_that("{shinytest2} recording: corrupted_data", {
  app <- AppDriver$new(name = "corrupted_data", height = 738, width = 1235)
  app$expect_values()
  app$upload_file(dataset = "data_corrupted.csv")
  app$click("clear")
})