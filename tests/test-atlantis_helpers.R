library(mfdbatlantis)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

day_secs <- 60 * 60 * 24
month_secs <- day_secs * 30

ok_group("atlantis_time_to_years", local({
    ok(cmp(
        atlantis_time_to_years(c(0, day_secs, day_secs * 365, day_secs * 365 * 2)),
        c(0, 0, 1, 2)
        ), "atlantis_time_to_years")
}, asNamespace('mfdbatlantis')))

ok_group("atlantis_time_to_months", local({
    ok(cmp(
        atlantis_time_to_months(c(
            0,
            day_secs * 29,
            day_secs * 30,
            day_secs * 90,
            day_secs * 364,
            month_secs * 12 + day_secs * 30,
            day_secs * 365)),
        c(
            1,
            1,
            2, # Made it to next month
            4, # Made it to month four
            13,  # Remainder 5 days are in month "13"
            1,  # Roll over to year 2
            1)  # Roll over to year 2
        ), "atlantis_time_to_months")
}, asNamespace('mfdbatlantis')))

ok_group("atlantis_time_to_days", local({
    ok(cmp(
        atlantis_time_to_days(c(
            0,
            day_secs * 29,
            day_secs * 30,
            day_secs * 365)),
        c(
            1,
            30,
            1,
            1)
        ), "atlantis_time_to_days")
}, asNamespace('mfdbatlantis')))
