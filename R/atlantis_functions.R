# Read required details in from bgm file
atlantis_read_areas <- function (adir) {
    get_box_attribute <- function (bgm_lines, field_name, new_name = field_name) {
        # Extract lines we are interested and break up into key/val
        rv <- grep(paste0("^[A-Za-z0-9]+\\.", field_name), bgm_lines, value = TRUE)
        rv <- strsplit(rv, paste0(".", field_name, "\\s+"))

        # Convert to data.frame & transpose
        rv <- t(as.data.frame(rv))
        rownames(rv) <- NULL
        colnames(rv) <- c("id", new_name)
        return(rv)
    }

    # Extract parts of file we are interested in
    bgm_lines <- readLines(attr(adir, 'bgm_area'))
    area_data <- merge(
        get_box_attribute(bgm_lines, "label", "name"),
        get_box_attribute(bgm_lines, "area", "size"),
        sort = FALSE)
    area_data$id <- seq_len(nrow(area_data))
    area_data$size <- as.numeric(area_data$size)
    return(area_data)
}

atlantis_functional_groups <- function (adir) {
    fg_doc <- XML::xmlParse(attr(adir, 'xml_fg'))
    fg_data <- fetch_xml_attributes(fg_doc, 'FunctionalGroup', c('GroupCode', 'Name', 'LongName', 'IsPredator', 'IsTurnedOn', 'NumCohorts', 'NumStages', 'NumAgeClassSize'), stringsAsFactors = FALSE)

    # Pull out useful flags from biology file and combine
    xmlAllAttrs <- Vectorize(XML::xmlAttrs)
    bio_doc <- XML::xmlParse(attr(adir, 'xml_bio'))
    for (flag in c('FLAG_AGE_MAT', 'FLAG_LI_A', 'FLAG_LI_B')) {
        bio_flags <- xmlAllAttrs(XML::getNodeSet(bio_doc, paste0("//Attribute[@AttributeName='", flag, "']/GroupValue")))
        flag_table <- data.frame(
            GroupCode = bio_flags["GroupName",],
            Value = as.numeric(bio_flags["AttributeValue",]),
            stringsAsFactors = TRUE)
        names(flag_table)[[2]] <- flag
        fg_data <- merge(fg_data, flag_table, all.x = TRUE, sort = FALSE)
    }

    return(fg_data)
}

atlantis_run_options <- function (adir) {
    opt_doc <- XML::xmlParse(attr(adir, 'xml_opt'))
    opt_data <- fetch_xml_attributes(opt_doc, "ScenarioOptions", c("dt"))

    return(opt_data)
}
