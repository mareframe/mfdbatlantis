\name{R/atlantis_fisheries.R}
\alias{atlantis_fisheries}
\alias{atlantis_fisheries_catch}

\title{Atlantis stomach content parsers}
\description{
    Routines to read and make use of the atlantis diet matrix
}

\usage{
atlantis_fisheries(adir)
atlantis_fisheries_catch(adir,
        area_data,
        fishery)
}

\arguments{
    \item{adir}{The atlantis directory created by \link{atlantis_directory}}
    \item{area_data}{The results of \link{atlantis_read_areas}}
    \item{fishery}{A single fishery from \link{atlantis_fisheries}}
}

\value{
\code{atlantis_fisheries} returns a data.frame describing each fishery in the
Atlantis output.

\code{atlantis_fisheries_catch} returns logbook output that can be ingested
into MFDB.
}
