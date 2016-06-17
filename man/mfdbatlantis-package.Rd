\name{mfdbatlantis-package}
\alias{mfdbatlantis-package}
\docType{package}

\title{MFDB Atlantis Interface}

\description{
Tools to extract data from Atlantis model output and transform it into forms
suitable for ingesting into MFDB.
}

\section{Overview}{
The mfdbatlantis package contains the following groups of functions:

\describe{
    \item{atlantis_directory}{
        Open a model directory, define names of common atlantis output files.
    }
    \item{atlantis_read_areas, atlantis_functional_groups}{
        Read in area and functional group definitions, for use in other
        functions.
    }
    \item{atlantis_tracer_*, atlantis_temperature}{
        Read the tracer files to generate temperature and simulated survey
        outputs, ready for ingesting into MFDB.
    }
    \item{atlantis_fisheries_*}{
        Read in catch data from defined Atlantis fisheries, so it can be
        uploaded as logbook data into MFDB.
    }
    \item{atlantis_stomach_*}{
        Expand output from a simulated survey to include stomach content data.
    }
}

The best way to undestand the usage is to read through the supplied demo
scripts in the \href{../demo}{the demo directory}.
}

\author{
Jamie Lentin

Maintainer: Jamie Lentin <jamie.lentin@shuttlethread.com>
}

\seealso{
\href{https://github.com/mareframe/mfdb}{mfdb},
\href{https://github.com/hafro/rgadget}{rgadget},
\href{http://www.hafro.is/gadget/userguide/userguide.html}{Gadget user guide}
}
