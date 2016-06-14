library(mfdb)
library(mfdbatlantis)
library(utils)

mdb <- mfdb('Atlantis-LVic')

lv_dir <- atlantis_directory('atlantis-L_Vic-2016-05-05')

lv_run_options <- atlantis_run_options(lv_dir)

# Read in areas / surface temperatures, insert into mfdb
lv_area_data <- atlantis_read_areas(lv_dir)
lv_temp <- atlantis_temperature(lv_dir, lv_area_data)
mfdb_import_area(mdb, lv_area_data)
mfdb_import_temperature(mdb, lv_temp[lv_temp$depth == 1,])

# Read in all functional groups, assign MFDB shortcodes where possible
lv_functional_groups <- atlantis_functional_groups(lv_dir)
lv_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(lv_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# Ingest survey data
for (fgName in c()) {  # TODO: Add some functional groups
    fg_group <- lv_functional_groups[c(lv_functional_groups$Name == fgName),]
    cat("Importing functional group", fg_group$Name, "\n")

    lv_fg_count <- atlantis_fg_tracer(lv_dir, lv_area_data, fg_group)
    lv_fg_count$species <- fg$MfdbCode
    lv_fg_count$areacell <- lv_fg_count$area
    mfdb_import_survey(mdb, lv_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

lv_fisheries <- atlantis_fisheries(lv_dir)
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = lv_fisheries$Index,
    name = lv_fisheries$Code,
    full_hame = lv_fisheries$Name,
    stringsAsFactors = FALSE))

for (fisheryCode in lv_fisheries$Code) {
    fishery <- lv_fisheries[lv_fisheries$Code == fisheryCode,]
    cat("Importing fishery", fisheryCode, "\n")

    lv_catch <- atlantis_fisheries_catch(lv_dir, lv_area_data, fishery)
    if (nrow(lv_catch) == 0) next

    # Species column that maps to MFDB code
    lv_catch$species <- lv_catch$functional_group
    levels(lv_catch$species) <- lv_functional_groups[match(
        levels(lv_catch$functional_group),
        lv_functional_groups$GroupCode), 'MfdbCode']

    mfdb_import_survey(mdb, data.frame(
        year = lv_catch$year,
        month = lv_catch$month,
        areacell = lv_catch$area,
        vessel = lv_catch$fishery,
        species = lv_catch$species,
        weight_total = lv_catch$weight_total,
        count = c(NA),
        stringsAsFactors = TRUE), data_source = paste0("atlantisFishery_", fisheryCode))
}
