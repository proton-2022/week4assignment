#' Read data from a csv file and constructs a tibble. 
#'
#' @param filename A character specifying the csv file to read. A non-existing 
#' filename will produce an error. 
#' @return a tibble containing the csv data. 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' fn <- "data/accident_2013.csv.bz2"
#' data <- fars_read(fn)
#' # filename that does not exist gives an error. 
#' \dontrun{
#' fn <- NULL
#' data <- fars_read(fn)
#' }
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generates a filename for a specific year. 
#' 
#' @param year A character or numeric specifying a year. Input \code{year} that 
#' cannot be casted to an integer will result in an error or an empty return array. 
#' @return A string in the format "accidient_%d.csv.bz2". 
#' @examples 
#' fn <- make_filename(year="2020")  
#' fn <- make_filename(year=2020)
#' # input values that cannot be casted to an integer will result in an error or 
#' # empty return array 
#' \dontrun{
#' fn <- make_filename(year=NULL)
#' fn <- make_filename(year="hello")
#' }
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Returns occurrence of accidents in a specific year. 
#' 
#' @param years A vector (atomic or list) of or a single numeric or character 
#' specifying years of accidents to return. Giving \code{years} that cannot 
#' be casted to a numeric, or for which no data exist will return NULL and 
#' a warning message. 
#' @return A tibble with 'MONTH' and 'year' columns specifying the month of 
#' accidents in the year. 
#' @importFrom dplyr mutate select 
#' @importFrom magrittr %>%
#' @examples
#' dat <- fars_read_years(2013)
#' dat <- fars_read_year(c(2013, 2014))
#' dat <- fars_read_year("2013")
#' \dontrun{
#' dat <- fars_read_year(c(2013, "hello")) # returns NULL for "hello"
#' }
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarizes the number of accidents in each month for specific years. 
#' 
#' @param years A vector (atomic or list) of or a single numeric or character 
#' specifying years of accidents to return. \code{years} input for which no 
#' data exist or that cannot be casted to a year will be skipped. An error 
#' will occur when no valid inputs are provided.
#' @return A tibble that summarizes that number of accidents in the years 
#' for which data exist. 
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @examples 
#' summary_table <- fars_summarize_years(2013)
#' summary_table <- fars_summarize_years(c(2013, 2014))
#' summary_table <- fars_summarize_years("2013")
#' 
#' # no column will be returned for invalid inputs
#' summary_table <- fars_summarize_years(c(2013, "hello"))
#' \dontrun{
#' summary_table <- fars_summarize_years("hello") # will result in an error
#' }
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the locations of accidents for a specific state in a year. 
#' 
#' @param state.num A value that indicates a state ID. Must be able to be casted
#' to an integer between 1 and 56. Error will occur if \code{year} is not a single, 
#' valid value or there is no existing data for that year; or if \code{state.num} 
#' is not a valid state number.
#' @param year A value that specifies a year. Must be able to be casted to an 
#' integer that specifies a valid year. 
#' @return None. Draws a map a of a state and points indicating locations of 
#' accidents, if there are accidents to plot. Otherwise draw nothing. 
#' @importFrom dplyr filter 
#' @importFrom maps map 
#' @importFrom graphics points
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(19, 2014)
#' \dontrun{
#' # error will be produced: 
#' fars_map_state(100, 2013) # invalid state.num
#' fars_map_state(1, 2017) # invalid year 
#' }
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
