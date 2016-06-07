library(mfdb)
library(mfdbatlantis)
library(utils)

mdb <- mfdb('Atlantis-Iceland')

# Find all the files in this directory
ice_dir <- 'atlantis-Iceland-NoFishing20150909-1'  # TODO: Shouldn't be in the demo either
attr(ice_dir, 'start_year') <- 1948

ice_run_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')

# Read in areas / surface temperatures, insert into mfdb
ice_area_data <- atlantis_read_areas(ice_dir, 'atlantis_L93.bgm')
ice_temp <- atlantis_tracer(ice_dir, 'OutputNoFish.nc', ice_area_data, 'Temp')
mfdb_import_area(mdb, ice_area_data)
ice_temp$temperature <- ice_temp$value
mfdb_import_temperature(mdb, ice_temp[ice_temp$depth == 1,])

# Read in all functional groups, and ingest data in turn
ice_functional_groups <- atlantis_functional_groups(ice_dir, 'GroupsIceland.xml', 'BiologyNoFish.xml')
for (fgName in c("Cod", "Haddock")) {
    fg <- ice_functional_groups[c(ice_functional_groups$Name == fgName),]
    cat("Importing functional group", as.character(fg$Name), "\n")

    speciesCode <- unlist(mfdb_find_species(as.character(fg$LongName))['name',])
    if (length(speciesCode) != 1) {
        stop("No direct species match: ", speciesCode)
    }
    ice_fg_count <- atlantis_fg_count(ice_dir, 'OutputNoFish.nc', ice_area_data, fg)
    ice_fg_count$species <- speciesCode
    ice_fg_count$areacell <- ice_fg_count$area
    mfdb_import_survey(mdb, ice_fg_count, data_source = paste0('atlantisTracer_', fgName))
}

fgName <- 'Cod'
fg <- ice_functional_groups[c(ice_functional_groups$Name == fgName),]
fg_species <- unlist(mfdb_find_species(as.character(fg$LongName))['name', 1])

# Fetch consumption and tracer indexes for functional group
consumption <- atlantis_consumption(
    ice_dir,
    ice_area_data,
    fg_group = fg,
    ingestion_period = c('Cod' = 4),
    start_year = 1948)

# Reduce area down to a pretend survey time/area
consumption <- consumption[consumption$year == 1950 & consumption$area %in% c("Box20", "Box21", "Box22"),]
# Assume we only catch 0.0001% of possible available
consumption$count <- round(consumption$count * 0.000001)

# Convert this into the 2 data.frames import_stomach requires
stomach <- atlantis_stomach_content(ice_dir, consumption, predator_map = c(
    FCD = 'COD'
), prey_map = c(
    # We're only interested in 2 species
    FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
    FCA = mfdb_find_species('Capelin')['name',][[1]]
))
mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = "stomach_Cod")
