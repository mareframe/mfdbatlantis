\name{R/atlantis_directory.R}
\alias{atlantis_directory}

\title{Open an Atlantis output directory}
\description{Open Atlantis output directory, find required files}

\usage{
atlantis_directory(path,
        bgm_area = first_file(path, '*.bgm'),
        nc_catch = first_file(path, "*CATCH.nc"),
        nc_out = first_file(path, "*.nc"),
        nc_prod = first_file(path, "*PROD.nc"),
        txt_diet = first_file(path, "*DietCheck.txt"),
        xml_bio = first_file(path, '*[Bb]io*.xml'),
        xml_fg = first_file(path, '*Groups*.xml'),
        xml_fisheries = first_file(path, '*Fisheries*.xml'),
        xml_opt = first_file(path, '*[rR]un*.xml'),
        start_year = 1948)
}

\arguments{
    \item{path}{Directory containing model output}
    \item{bgm_area}{The BGM file that defines model boxes}
    \item{nc_catch}{The .nc file containing catch data}
    \item{nc_out}{The .nc file containing count and StructN}
    \item{nc_prod}{The .nc file containing Eat}
    \item{txt_diet}{The diet matrix}
    \item{xml_bio}{XML biology file}
    \item{xml_fg}{XML functional group definitions}
    \item{xml_fisheries}{XML fisheries definitions}
    \item{xml_opt}{XML run options}
    \item{start_year}{The year the model is assumed to start at}
}

\details{
All aguments apart from path are optional. If not supplied then it makes a best
guess of the filename and errors if it can't be found.
}

\value{The path to the directory, annotated with filenames}
