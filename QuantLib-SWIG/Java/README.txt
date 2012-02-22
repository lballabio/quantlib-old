
Instructions for building the QuantLib-Java module under Eclipse are
available at
<http://users.telenet.be/johan.witters/website/skwash.com/get/get3.html>.

On Linux systems, the module can be build by supplying the location of
the JDK to configure, as in (for example)

./configure --with-jdk-include=/usr/lib/jvm/java-1.5.0-sun-1.5.0.08/include \
  --with-jdk-system-include=usr/lib/jvm/java-1.5.0-sun-1.5.0.08/include/linux

and by running 'make' afterwards.

