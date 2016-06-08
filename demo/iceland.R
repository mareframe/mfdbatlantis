library(mfdb)
library(mfdbatlantis)
library(utils)

mdb <- mfdb('Atlantis-Iceland')

is_dir <- atlantis_directory('atlantis-Iceland-2016-05-24')

is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)
mfdb_import_area(mdb, is_area_data)
mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# Ingest survey data
for (fgName in c("Cod", "Haddock")) {
    fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
    cat("Importing functional group", fg$Name, "\n")

    is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)
    is_fg_count$species <- fg$MfdbCode
    is_fg_count$areacell <- is_fg_count$area
    # TODO: More selectivity (Look with Bjarki at Rgadget mean length simulat)
    # TODO: Want a length selectivity curve for the gear, areas that you're interested in.
    mfdb_import_survey(mdb, is_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

for (fgName in c("Cod")) {
    fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]

    # Fetch consumption and tracer indexes for functional group
    consumption <- atlantis_fg_tracer(
        is_dir,
        is_area_data,
        fg_group = fg_group,
        consumption = TRUE)

    # Only survey the first quarter, and in 3 boxes
    consumption <- consumption[consumption$month == 1 & consumption$area %in% c("Box20", "Box21", "Box22"),]
    # TODO: Sample every nth fish (that the survey takes?)
    #   => Don't bother with this, only generating proportions anyway

    # Convert this into the 2 data.frames import_stomach requires
    stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
        FCD = 'COD'
    ), prey_map = c(
        # We're only interested in 2 species
        FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
        FCA = mfdb_find_species('Capelin')['name',][[1]]
    ))
    mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = "stomach_Cod")
}

is_fisheries <- atlantis_fisheries(is_dir)
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = is_fisheries$Index,
    name = is_fisheries$Code,
    full_name = is_fisheries$Name,
    stringsAsFactors = FALSE))

for (fisheryCode in is_fisheries$Code) {
    fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]
    cat("Importing fishery", fisheryCode, "\n")

    is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
    if (nrow(is_catch) == 0) next

    # Species column that maps to MFDB code
    is_catch$species <- is_catch$functional_group
    levels(is_catch$species) <- is_functional_groups[match(
        levels(is_catch$functional_group),
        is_functional_groups$GroupCode), 'MfdbCode']

    mfdb_import_survey(mdb, data.frame(
        year = is_catch$year,
        month = is_catch$month,
        areacell = is_catch$area,
        vessel = is_catch$fishery,
        species = is_catch$species,
        weight_total = is_catch$weight_total,
        count = c(NA),
        stringsAsFactors = TRUE), data_source = paste0("atlantisFishery_", fisheryCode))
}
