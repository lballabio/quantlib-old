This is an experimental SWIG interface between perl and QuantLib. See
<joe@confucius.gnacademy.org> for more details.

Running 'make' in this directory will build the module; 'make install'
will make it available on your system.c

Currently, example code segfaults if statements like this are executed:

my($riskFreeRate) = new QuantLib::FlatForward($settlementDate,
                                              0.05,
                                              new QuantLib::Actual365Fixed);

Instead, one has to write the above as

my($dayCount) = new QuantLib::Actual365Fixed;
my($riskFreeRate) = new QuantLib::FlatForward($settlementDate,
                                              0.05,
                                              $dayCount);

so that it runs---after which, it will segfault upon exiting the script.
