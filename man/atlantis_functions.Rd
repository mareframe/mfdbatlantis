\name{R/atlantis_functions.R}
\alias{atlantis_read_areas}
\alias{atlantis_functional_groups}
\alias{atlantis_run_options}

\title{Atlantis misc. parsers}
\description{
    Routines to read and make use of misc. Atlantis files
}

\usage{
atlantis_read_areas(adir)
atlantis_functional_groups(adir)
atlantis_run_options(adir)
}

\arguments{
    \item{adir}{The atlantis directory created by \link{atlantis_directory}}
}

\value{
\code{atlantis_read_areas} returns a \code{data.frame} describing the boxes
defined in the model. The output is both designed to be ingested into MFDB and
used in various other functions.

\code{atlantis_functional_groups} returns a \code{data.frame} describing the
functional groups in the model. The output is used in various other functions.

\code{atlantis_run_options} returns a \code{data.frame} with some values from
the run file.
}