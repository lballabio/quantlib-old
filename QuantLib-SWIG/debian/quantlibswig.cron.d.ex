#
# Regular cron jobs for the quantlibswig package
#
0 4	* * *	root	[ -x /usr/bin/quantlibswig_maintenance ] && /usr/bin/quantlibswig_maintenance
