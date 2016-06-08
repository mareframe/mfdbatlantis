# Select the first file matching the glob in path
first_file <- function (path, glob) {
    files <- sort(Sys.glob(file.path(path, glob)))
    files <- files[order(nchar(files), files)]
    if (length(files) == 0) stop("No files found for ", c(path, glob))
    return(files[[1]])
}

atlantis_directory <- function(path,
        bgm_area = first_file(path, '*.bgm'),
        nc_catch = first_file(path, "*CATCH.nc"),
        nc_out = first_file(path, "*.nc"),
        nc_prod = first_file(path, "*PROD.nc"),
        txt_diet = first_file(path, "*DietCheck.txt"),
        xml_bio = first_file(path, '*[Bb]io*.xml'),
        xml_fg = first_file(path, '*Groups*.xml'),
        xml_fisheries = first_file(path, '*Fisheries*.xml'),
        xml_opt = first_file(path, '*[rR]un*.xml'),
        start_year = 1948) {
    adir <- structure(
        path,
        bgm_area = bgm_area,
        nc_catch = nc_catch,
        nc_out = nc_out,
        nc_prod = nc_prod,
        txt_diet = txt_diet,
        xml_bio = xml_bio,
        xml_fg = xml_fg,
        xml_fisheries = xml_fisheries,
        xml_opt = xml_opt,
        start_year = start_year,
        class = append(class(""), "atlantis_directory"))
    return(adir)
}
