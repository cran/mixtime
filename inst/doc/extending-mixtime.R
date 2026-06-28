## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(mixtime)

## -----------------------------------------------------------------------------
.onLoad <- function(...) {
  S7::methods_register()
}

## ----time-units---------------------------------------------------------------
# Timezone-aware Symmetry454 year and month units
new_time_unit("tu_symmetry454_year",  parent = mt_tz_unit)
new_time_unit("tu_symmetry454_month", parent = mt_tz_unit)

## ----new-calendar-------------------------------------------------------------
cal_symmetry454 <- new_calendar(
  year  = new_time_unit("tu_symmetry454_year",  parent = mt_tz_unit),
  month = new_time_unit("tu_symmetry454_month", parent = mt_tz_unit),
  week  = cal_isoweek$week,
  # Inherit civil-time units (day, hour, minute, second, ...)
  inherit = cal_time_civil,
  class = "cal_symmetry454"
)

cal_symmetry454

## ----calendar-access----------------------------------------------------------
cal_symmetry454$year(1L)
cal_symmetry454$month(1L, tz = "UTC")

## -----------------------------------------------------------------------------
chronon_cardinality(cal_isoweek$day(1L), cal_isoweek$week(1L))

## -----------------------------------------------------------------------------
chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(1L), at = 0L) # Jan 1970
chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(1L), at = 1L) # Feb 1970
chronon_cardinality(cal_gregorian$day(1L), cal_gregorian$month(1L), at = 25L) # Feb 1972 (leap year)

## ----cardinality-fixed--------------------------------------------------------
# Each Symmetry454 year has 12 months
S7::method(chronon_cardinality, list(cal_symmetry454$month, cal_symmetry454$year)) <-
  function(x, y, at = NULL) {
    y@n * 12L / x@n
  }
chronon_cardinality(cal_symmetry454$month(1L), cal_symmetry454$year(1L))

## ----cardinality-variable-----------------------------------------------------
S7::method(chronon_cardinality, list(cal_symmetry454$week, cal_symmetry454$month)) <-
  function(x, y, at = NULL) {
    # The number of weeks in each n-month period
    month_size <- y@n
    nweeks_cyc <- circsum(c(4L, 5L, 4L), month_size)

    # Find which n-month period we're in based on the "at" position (months since epoch)
    period <- at %% length(nweeks_cyc) + 1L

    nweeks <- nweeks_cyc[period]

    # Add the extra week to December for Symmetry454 leap years.
    # A year is a leap year when (52*year + 146) %% 293 < 52.
    m1 <- at * month_size
    contains_dec <- which((m1 %% 12L) >= (12L - month_size))
    year <- 1970L + m1[contains_dec] %/% 12L
    is_leap_year <- ((52 * year + 146L) %% 293L) < 52L

    nweeks[contains_dec[is_leap_year]] <- nweeks[contains_dec[is_leap_year]] + 1L

    # Scale by the number of weeks in the n-week time granule
    nweeks / x@n
  }

# The 4-5-4 cycle across a full Symmetry454 year
chronon_cardinality(cal_symmetry454$week(1L), cal_symmetry454$month(1L), at = 0:11)

# 1970 is a leap year (53 weeks), so December has 5 weeks instead of 4
chronon_cardinality(cal_symmetry454$week(1L), cal_symmetry454$month(1L), at = 11L)

# Non-leap year: December has the usual 4 weeks
chronon_cardinality(cal_symmetry454$week(1L), cal_symmetry454$month(1L), at = 12:23) # Dec 1971

# The number of weeks in a multi-month period is the sum of the weeks in each month
chronon_cardinality(cal_symmetry454$week(1L), cal_symmetry454$month(2L), at = 0:5)

## -----------------------------------------------------------------------------
# The number of weeks in a Symmetry454 year (derived from week → month → year)
# Note that the leap year in 1970 (at = 0) produces 53 weeks via week → month.
chronon_cardinality(cal_symmetry454$week(1L), cal_symmetry454$year(1L), at = 0:4)

# The number of days in a Symmetry454 year (derived from day → week → month → year)
chronon_cardinality(cal_symmetry454$day(1L), cal_symmetry454$year(1L), at = 0:4)

## -----------------------------------------------------------------------------
# The number of Symmetry454 years in a month is the inverse of months → years (1/12)
chronon_cardinality(cal_symmetry454$year(1L), cal_symmetry454$month(1L))

# The number of Symmetry454 months in 2 weeks (requires `at` since weeks → months is irregular)
chronon_cardinality(cal_symmetry454$month(1L), cal_symmetry454$week(2L), at = 0:4)

## -----------------------------------------------------------------------------
chronon_divmod(cal_gregorian$day(1L), cal_gregorian$month(1L), 45L)

## ----divmod-------------------------------------------------------------------
S7::method(chronon_divmod, list(cal_symmetry454$week, cal_symmetry454$month)) <-
  function(from, to, x) {
    # Most of this code works on 1-week granules
    week_size <- from@n
    x <- x * week_size  # convert n-weeks to 1-weeks

    # 1. Account for leap weeks by regularising x to have a fixed 52 weeks per year

    # The symmetrical sub-cycles of the 293-year leap week cycle are:
    #   17+11+17 + 17+17+11+17+17 + 17+11+17 + 17+17+11+17+17 + 17+11+17
    #   = 45 + 79 + 45 + 79 + 45 = 293
    # Primary (length 17) sub-cycles have 3 leap years: 00100000100000100
    # Secondary (length 11) sub-cycles have 2 leap years: 00100000100
    leaps_cycle_17 <- function(w) (w >= 157L) + (w >= 470L) + (w >= 783L)
    leaps_cycle_11 <- function(w) (w >= 157L) + (w >= 470L)
    leaps_cycle_45 <- function(w) {
      ifelse(w < 887L,  leaps_cycle_17(w),
      ifelse(w < 1461L, 3L + leaps_cycle_11(w - 887L),
                        5L + leaps_cycle_17(w - 1461L)))
    }
    leaps_cycle_79 <- function(w) {
      ifelse(w < 887L,  leaps_cycle_17(w),
      ifelse(w < 1774L, 3L  + leaps_cycle_17(w - 887L),
      ifelse(w < 2348L, 6L  + leaps_cycle_11(w - 1774L),
      ifelse(w < 3235L, 8L  + leaps_cycle_17(w - 2348L),
                        11L + leaps_cycle_17(w - 3235L)))))
    }
    leaps_symmetry454 <- function(x) {
      # There are 15288 weeks in a full 293-year cycle (293*52 + 52 leap weeks)
      w <- x %% 15288L
      x %/% 15288L * 52L +
      ifelse(w < 2348L,  leaps_cycle_45(w),
      ifelse(w < 6470L,  8L  + leaps_cycle_79(w - 2348L),
      ifelse(w < 8818L,  22L + leaps_cycle_45(w - 6470L),
      ifelse(w < 12940L, 30L + leaps_cycle_79(w - 8818L),
                         44L + leaps_cycle_45(w - 12940L)))))
    }

    # Offset x to align with the nearest 293-year cycle boundary before the epoch.
    # There are 349 leap years between year 1-W1 and the 1970-W1 epoch, and the
    # nearest cycle start is (1969*52 + 349) %% (293*52 + 52) = 11009 weeks before epoch.
    x_cyc <- x + 11009L + week_size  # right align multi-week granules
    n_leaps <- leaps_symmetry454(x_cyc)

    # Regularise x to have exactly 52 weeks per year by subtracting leap weeks.
    # (37 leap years occur in the cycle before the epoch, so we add 37 back.)
    x_reg <- x - n_leaps + 37L

    # 2. Use the 4-5-4 pattern to find the month (div) and week remainder (mod)
    ## The number of weeks in each n-month period
    month_size <- to@n
    weeks_len <- circsum(c(4L, 5L, 4L), month_size)

    ## The total weeks in a full n-month cycle
    weeks_tot <- sum(weeks_len)

    ## Find which n-month cycle we're in based on the regularised week count
    period_full <- x_reg %/% weeks_tot

    ## Find which part within the n-month cycle we're in
    weeks_seq  <- cumsum(weeks_len[-length(weeks_len)])
    period_part <- rowSums(outer(x_reg %% weeks_tot, weeks_seq, ">="))

    # div: total complete n-month cycles + complete n-months within the current cycle
    div <- period_full * length(weeks_len) + period_part
    # mod: remaining (regularised) weeks within the current n-month period
    mod <- x_reg %% weeks_tot - c(0L, weeks_seq)[period_part + 1L]


    # 3. Adjust the remainder to account for leap weeks that were removed during regularisation.

    # Identify leap weeks re-using cumulative in-cycle leap week counts: leaps_symmetry454()
    # If the week added a leap week from the previous, then it itself must be a leap week.
    # Applied only to regularised 52/53rd weeks of the year (only these weeks can be leap weeks)
    last_weeks  <- which(x_reg %% 52L >= 51L)
    leap_weeks  <- last_weeks[n_leaps[last_weeks] - leaps_symmetry454(x_cyc[last_weeks] - week_size) > 0L]
    mod[leap_weeks] <- mod[leap_weeks] + 1L  # restore leap week to remainder

    # Scale mod back to the original n-unit week size
    mod <- mod %/% week_size

    # Return the divmod result
    list(div = div, mod = mod)
  }

## -----------------------------------------------------------------------------
# Week 19 (0-indexed) of 1970 is the 2nd week (div=1) of May 1970 (mod=4)
with(cal_symmetry454, chronon_divmod(week(1L), month(1L), 18L))

# Week 52 (0-indexed) is the leap week of 1970; it is the 5th week (div=4) of Dec 1970 (mod=11)
with(cal_symmetry454, chronon_divmod(week(1L), month(1L), 52L))

## -----------------------------------------------------------------------------
# The 5th fortnight of 1970 is the 3rd fortnight (div=2) of Feb 1970 (mod=1)
with(cal_symmetry454, chronon_divmod(week(2L), month(1L), 4L))

## -----------------------------------------------------------------------------
# Divmod for converting days → years is derived from days → weeks → months → years
# Gregorian day 839 since unix epoch (1972-04-19) is symmetry454 day 101 (mod=100) of year 1972 (div=2)
with(cal_symmetry454, chronon_divmod(day(1L), year(1L), 839L))

## -----------------------------------------------------------------------------
S7::method(chronon_epoch, cal_symmetry454$year) <- function(x) 1970L

## -----------------------------------------------------------------------------
S7::method(time_unit_full, cal_symmetry454$year)  <- function(x) "Symmetry454 year{?/s}"
S7::method(time_unit_abbr, cal_symmetry454$year)  <- function(x) "Y"
S7::method(time_unit_full, cal_symmetry454$month) <- function(x) "Symmetry454 month{?/s}"
S7::method(time_unit_abbr, cal_symmetry454$month) <- function(x) "M"

## -----------------------------------------------------------------------------
linear_time(as.Date("1955-11-12"), chronon = cal_symmetry454$year(1L))

## -----------------------------------------------------------------------------
year(as.Date("1955-11-12"), calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
# Week of the month
cyclical_time(as.Date("1955-11-12"), chronon = week(1L), cycle = month(1L), calendar = cal_symmetry454)

# Month of the year
month_of_year(as.Date("1955-11-12"), calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
S7::method(linear_labels, cal_symmetry454$year) <- function(granule, i, ...) {
  ifelse(i <= 0L, paste0(-i + 1L, "BC"), i)
}

## -----------------------------------------------------------------------------
year(-1:2, calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
# Labels for months of the year, essentially the same as Gregorian months in years.
S7::method(cyclical_labels, list(cal_symmetry454$month, cal_symmetry454$year)) <-
  function(granule, cycle, i, label = FALSE, abbreviate = FALSE, ...) {
    if (label) {
      # Index into R's localised month name objects (month.name and month.abb)
      if (abbreviate) month.abb[i + 1L] else month.name[i + 1L]
    } else {
      # Use i + 1L for 1-indexing months (so January is 1, February is 2, ...)
      sprintf("%02d", i + 1L)
    }
  }

# Labels for weeks of the month are simply 1-indexed (e.g. "W1", "W2", ...)
S7::method(cyclical_labels, list(cal_symmetry454$week, cal_symmetry454$month)) <-
  function(granule, cycle, i, ...) {
    as.character(i + 1L)
  }

## -----------------------------------------------------------------------------
# Month of year
month_of_year(as.Date("1955-11-12"), calendar = cal_symmetry454)
# Week of month
cyclical_time(as.Date("1955-11-12"), chronon = cal_symmetry454$week(1L), cycle = cal_symmetry454$month(1L))

# Day of week (inherited from cal_isoweek)
day_of_week(as.Date("1955-11-12"), calendar = cal_symmetry454)
# Day of year (inherited from cal_time_civil)
day_of_year(as.Date("1955-11-12"), calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
date(as.Date("1955-11-12"), calendar = cal_symmetry454)
yearweek(as.Date("1955-11-12"), calendar = cal_symmetry454)
yearmonth(as.Date("1955-11-12"), calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
# Simply display years as numbers (e.g. "1970", "1971", ...)
S7::method(
  chronon_format_linear,
  list(cal_symmetry454$year, S7::class_any)
) <- function(x, cal) "{lin(year(1L))}"

# Display labelled months as month within year (e.g. "1970 Jan", "1970 Feb", ...)
S7::method(
  chronon_format_linear,
  list(cal_symmetry454$month, S7::class_any)
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}"

# Display weeks as week in month in year (e.g. "1970-01-W1", "1970-01-W2", ...)
S7::method(
  chronon_format_linear,
  list(cal_symmetry454$week, S7::new_S3_class("cal_symmetry454"))
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}-W{cyc(week(1L), month(1L))}"

# Format days as day in week in month in year (e.g. "1970-Jan-W1-Mon", "1970-Jan-W1-Tue", ...)
S7::method(
  chronon_format_linear,
  list(cal_symmetry454$day, S7::new_S3_class("cal_symmetry454"))
) <- function(x, cal) "{lin(year(1L))}-{cyc(month(1L), year(1L), label=TRUE, abbreviate=TRUE)}-W{cyc(week(1L), month(1L))}-{cyc(day(1L), week(1L), label=TRUE, abbreviate=TRUE)}"

## -----------------------------------------------------------------------------
# Years are formatted as YYYY
year(as.Date("1955-11-12"), calendar = cal_symmetry454)

# Months are formatted as YYYY Mon
yearmonth(as.Date("1955-11-12"), calendar = cal_symmetry454)

# Weeks are formatted as YYYY-MM-WW
yearweek(as.Date("1955-11-12"), calendar = cal_symmetry454)

# Days are formatted as YYYY-MM-WW-DD
linear_time(as.Date(c("1985-10-26", "1955-11-05", "1955-11-12")), chronon = cal_symmetry454$day(1L))

## -----------------------------------------------------------------------------
# Format months in years as abbreviated month labels (e.g. "Jan", "Feb", ...)
S7::method(
  chronon_format_cyclical,
  list(cal_symmetry454$month, cal_symmetry454$year)
) <- function(x, y) "{cyc(month,year,label=TRUE,abbreviate=TRUE)}"

## -----------------------------------------------------------------------------
month_of_year(as.Date("1955-11-12"), calendar = cal_symmetry454)

## -----------------------------------------------------------------------------
duration(1L, cal_symmetry454$year(1L))
duration(3L, cal_symmetry454$month(1L))

