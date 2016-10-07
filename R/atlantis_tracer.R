atlantis_temperature <- function (adir,
        area_data) {
    nc_out <- ncdf4::nc_open(attr(adir, 'nc_out'))

    tracer <- ncdf4::ncvar_get(nc_out, 'Temp')
    dims <- expand.grid(
        depth = nc_out$dim$z$vals,
        areacell = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        stringsAsFactors = TRUE)

    data.frame(
        depth = dims$depth,
        areacell = dims$areacell,
        time = factor(dims$time),
        year = atlantis_time_to_years(dims$time) + attr(adir, 'start_year'),
        month = atlantis_time_to_months(dims$time),
        day = atlantis_time_to_days(dims$time),
        temperature = as.numeric(tracer),
        stringsAsFactors = TRUE)
}

atlantis_fg_tracer <- function (adir,
        area_data,
        fg_group,
        consumption = FALSE) {
    # Read in both counts and mgN of all cohorts in group
    nc_out <- ncdf4::nc_open(attr(adir, 'nc_out'))
    fg_Nums <- as.numeric(fetch_nc_variables(nc_out, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'Nums'))
    fg_StructN <- as.numeric(fetch_nc_variables(nc_out, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'StructN'))
    dims <- expand.grid(
        depth = nc_out$dim$z$vals,
        area = as.character(area_data$name),
        time = nc_out$dim$t$vals,
        cohort = seq_len(as.character(fg_group$NumCohorts)),
        stringsAsFactors = FALSE)
    weight_grams <- mgn_to_grams(ifelse(fg_Nums > 0, fg_StructN, NA))  # Per-individual, so we can treat it as mean weight
    df_out <- data.frame(
        depth = dims$depth,
        area = dims$area,
        year = atlantis_time_to_years(dims$time) + attr(adir, 'start_year'),
        month = atlantis_time_to_months(dims$time),
        day = atlantis_time_to_days(dims$time),
        group = as.character(fg_group$GroupCode),
        cohort = dims$cohort,
        weight = weight_grams,
        length = (weight_grams / fg_group$FLAG_LI_A) ^ (1 / fg_group$FLAG_LI_B),
        count = fg_Nums,
        stringsAsFactors = TRUE)

    if (consumption) {
        # Drop depth from data.frame
        df_out <- aggregate(cbind(weight, length, count) ~ area + year + month + group + cohort, df_out, sum)

        # Read in consumption data
        nc_prod <- ncdf4::nc_open(attr(adir, 'nc_prod'))
        fg_Eat <- fetch_nc_variables(nc_prod, paste0(fg_group$Name, seq_len(as.character(fg_group$NumCohorts))), 'Eat')
        dims <- expand.grid(
            area = as.character(area_data$name),
            time = nc_prod$dim$t$vals,
            cohort = seq_len(fg_group$NumCohorts),
            stringsAsFactors = FALSE)
        df_eat <- data.frame(
            area = dims$area,
            year = atlantis_time_to_years(dims$time) + attr(adir, 'start_year'),
            month = atlantis_time_to_months(dims$time),
            day = atlantis_time_to_days(dims$time),
            group = as.character(fg_group$GroupCode),
            cohort = dims$cohort,
            consumption = as.numeric(fg_Eat),
            stringsAsFactors = TRUE)

        df_out <- merge(df_out, df_eat, by = c('area', 'year', 'month', 'group', 'cohort'))
        # Consumption is mgN/day for the whole cohort. Divide to get rates for individual
        df_out$ind_consumption <- df_out$consumption / df_out$count
    }

    # Add age to data
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    df_out$age <- (df_out$cohort * age_class_size) - (age_class_size %/% 2 + 1)

    # Maturity stage is mature iff cohort greater than FLAG_AGE_MAT
    df_out$maturity_stage = ifelse(df_out$cohort > fg_group$FLAG_AGE_MAT, 5, 1)

    return(df_out)
}

# Expand length (mean) to buckets of length_groups
atlantis_tracer_add_lengthgroups <- function(
        tracer_data,  # Output of fg_tracer
        length_group,  # vector of c(min, min, min, ..., max)
        sigma_per_cohort  # length std.dev per age in functional group
        ) {

        if (length(length_group) < 2) {
            stop("Length group should have at least 2 members")
        }

        # No point considering records with no fish
        tracer_data <- tracer_data[tracer_data$count > 0,]

        # Add variance to tracer data
        tracer_data$length_var <- sigma_per_cohort[tracer_data$cohort] ^ 2

        # Similar to rgadget:::distr (R/function.R, line 99)
        lengrp_lower <- length_group[-length(length_group)]  # Upper bounds for length groups
        lengrp_upper <- length_group[-1]  # Lower bounds for length groups
        len_dist <- function (len) {
            pnorm(
                rep(len, each = nrow(tracer_data)),
                tracer_data$length,  # i.e. mean length
                sigma_per_cohort[tracer_data$cohort])
        }
        length_groups <- as.data.frame(matrix(
            rep(tracer_data$count, times = length(lengrp_upper)) * (len_dist(lengrp_upper) - len_dist(lengrp_lower)),
            dimnames = list(
                c(),
                paste("length", lengrp_lower, lengrp_upper, sep = "_")),
            ncol = length(lengrp_lower)))

        return(cbind(tracer_data, length_groups))
}

atlantis_tracer_survey_select <- function(
        tracer_data,  # Tracer data with length_(x)_(y) columns
        length_group,  # vector of c(min, min, min, ..., max) per functional group
        survey_suitability,  # Suitability vector, one entry per length group
        survey_sigma  # Error rate
        ) {
    lengrp_lower <- length_group[-length(length_group)]  # Upper bounds for length groups
    lengrp_upper <- length_group[-1]  # Lower bounds for length groups
    err_variance <- survey_sigma^2

    # Strip columns we will replace from tracer_data
    base_names <- grep(
        "^length|^weight|^count",
        names(tracer_data),
        value = TRUE, invert = TRUE)
    base_data <- tracer_data[, base_names, drop = FALSE]

    # For each length group, extract data.frame from tracer_data
    do.call(rbind, lapply(seq_len(length(lengrp_lower)), function (i) {
        length_col <- paste(
            "length",
            lengrp_lower[[i]],
            lengrp_upper[[i]],
            sep = "_")

        out <- base_data
        out$length <- mean(c(lengrp_upper[[i]], lengrp_lower[[i]]))
        out$weight <- tracer_data$weight
        out$count <- round(
            tracer_data[,length_col] *
            exp(rnorm(nrow(base_data), 0, err_variance) - err_variance/2) *
            survey_suitability[[i]])
        return(out)
    }))
}
