Mareframe Atlantis Helpers
==========================

Some functions to extract data from an Atlantis dump

MFDB Atlantis Interface
=======================

Tools to extract data from Atlantis model output and transform it into forms
suitable for ingesting into MFDB.

The mfdbatlantis package contains the following groups of functions:

* ``atlantis_directory``: Open a model directory, define names of common atlantis output files.
* ``atlantis_read_areas, atlantis_functional_groups``: Read in area and functional group definitions, for use in other functions.
* ``atlantis_tracer_*, atlantis_temperature``: Read the tracer files to generate temperature and simulated survey outputs, ready for ingesting into MFDB.
* ``atlantis_fisheries_*``: Read in catch data from defined Atlantis fisheries, so it can be uploaded as logbook data into MFDB.
* ``atlantis_stomach_*``: Expand output from a simulated survey to include stomach content data.

The best way to undestand the usage is to read through the supplied demo
scripts in the demo directory.

```
> library(mfdbatlantis)
> demo('lake-victoria', package='mfdbatlantis', ask = FALSE)
> demo('iceland', package='mfdbatlantis', ask = FALSE)
```

Acknowledgements
----------------
This project has received funding from the European Union’s Seventh Framework
Programme for research, technological development and demonstration under grant
agreement no.613571.

See Also
--------

* https://github.com/mareframe/mfdb
* https://github.com/hafro/rgadget
* http://www.hafro.is/gadget/userguide/userguide.html
