library(mfdb)
library(utils)

mfdb('', destroy_schema = TRUE)  # TODO: Not good in a demo.
mdb <- mfdb('Test')

# Find all the files in this directory
ice_dir <- 'atlantis-Iceland-NoFishing20150909-1'

ice_run_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')

# Read in areas / surface temperatures, insert into mfdb
ice_area_data <- atlantis_read_areas(ice_dir, 'atlantis_L93.bgm')
ice_temp <- atlantis_tracer(ice_dir, 'OutputNoFish.nc', ice_area_data, 'Temp')
mfdb_import_area(mdb, ice_area_data)
ice_temp$temperature <- ice_temp$value
mfdb_import_temperature(mdb, ice_temp[ice_temp$depth == 1,])
ice_functional_groups <- atlantis_functional_groups(ice_dir, 'GroupsIceland.xml', 'BiologyNoFish.xml')
ice_run_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')
ice_fg_count <- atlantis_fg_count(ice_dir, 'OutputNoFish.nc', ice_area_data,
    ice_functional_groups[c(ice_functional_groups$Name == 'Cod'),])
