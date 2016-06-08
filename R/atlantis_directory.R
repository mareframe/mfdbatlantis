atlantis_directory <- function(path, start_year = 1948) {
    adir <- structure(
        path,
        start_year = start_year,
        class = append(class(""), "atlantis_directory"))
    return(adir)
}
