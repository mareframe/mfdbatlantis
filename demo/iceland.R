ice_dir <- 'atlantis-Iceland-NoFishing20150909-1'
ice_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')
ice_area_data <- atlantis_read_areas(ice_dir, 'atlantis_L93.bgm')
ice_temp <- atlantis_tracer(ice_dir, 'OutputNoFish.nc', ice_area_data, 'Temp')
ice_functional_groups <- atlantis_functional_groups(ice_dir, 'GroupsIceland.xml', 'BiologyNoFish.xml')
ice_run_options <- atlantis_run_options(ice_dir, 'RunNoFish.xml')
ice_fg_count <- atlantis_fg_count(ice_dir, 'OutputNoFish.nc', ice_area_data,
    ice_functional_groups[c(ice_functional_groups$Name == 'Cod'),])
