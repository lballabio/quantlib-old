session files are meant to lauch Excel sessions dedicated to
specific QuantLibXL framework session. They also automate few
actions, e.g. bootstrapping rate curves

Each bat file goes along with the similarly named xml file.
As example the file

session_file.EUR-x64-dev-s-static

* bootstraps EUR rate curves
* uses -x64 binaries (if -x64 is missing win32 is assumed)
* -dev are developes' only files (with the path typical of
  a developer installations)
* -s is for static linking (QuantLibXL-*.xll only, without
  separate ObjectHandler-*.xll
* -static is for using static market data instaed of
  -live Reuters market data
