use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Regexp::English',
    'VERSION_FROM' => 'English.pm', # finds $VERSION
    'LIBS'	=> [''],   # e.g., '-lm' 
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'INC'	=> '',     # e.g., '-I/usr/include/other' 
	'PREREQ_PM' => { 'Test::More' => '0.30' },
	($] >= 5.005 ?    ## Add these new keywords supported since 5.005
		        (ABSTRACT_FROM => 'English.pm', # retrieve abstract from module
				AUTHOR     => 'chromatic <chromatic@wgz.org>') : ()),
);
