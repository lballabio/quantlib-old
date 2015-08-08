# File : Makefile.PL
use ExtUtils::MakeMaker;
WriteMakefile(
	      'NAME'    => 'QuantLib',         # Name of package
	      'LIBS'    => ['-lQuantLib'],     # Name of custom libraries
	      'OBJECT'  => 'quantlib_wrap.o',  # Object files
	      'MAKEFILE' => 'Makefile.quantlib',
	      'FIRST_MAKEFILE' => 'Makefile.quantlib',
	      'VERSION' => '1.7'
	      );
