
#' Add dummy design data
#'
#' If a design is missing in the data, include dummy entry
#' 
#' @param dat_raw 
#'
#' @return
#' @export
#'
add_dummy_design_data <- function(dat_raw) {
  
  if (!1 %in% dat_raw$Design.ID) {
    new_row <- data.frame(0, -1, "Randomized clinical trial", 1, 2, 2, 2, NA_integer_, 1, 1, NA_integer_, 1, 2, NA_integer_, NA_character_, NA_character_, NA_character_)
    names(new_row) <- names(dat_raw)
    dat_raw <- rbind(new_row, dat_raw)
  }
  if (!3 %in% dat_raw$Design.ID) {
    new_row <- data.frame(0, -3, "Prospective observational study", 3, 2, 2, 2, NA_integer_, 1, 1, NA_integer_, 1, 2, NA_integer_, NA_character_, NA_character_, NA_character_)
    names(new_row) <- names(dat_raw)
    dat_raw <- rbind(new_row, dat_raw)
  }
  if (!4 %in% dat_raw$Design.ID) {
    new_row <- data.frame(0, -4, "Test-negative", 4, 2, 2, 2, NA_integer_, 1, 1, NA_integer_, 1, 2, NA_integer_, NA_character_, NA_character_, NA_character_)
    names(new_row) <- names(dat_raw)
    dat_raw <- rbind(new_row, dat_raw)
  }
  
  dat_raw  
}
