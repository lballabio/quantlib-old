# Define some macros for the use of dealing with version dependent install
%define ruby %(which ruby)
%define crtArch %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['host_os']")
%define rbVersion %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['ruby_version']")
%define rbVMajor %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['MAJOR']")
%define rbVMinor %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['MINOR']")
%define rb_archdir %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['archdir']")
%define rb_libdir %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['rubylibdir']")
%define libdir %(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['libdir']")

Summary: The Ruby wrapper for the QuantLib library.
Name: QuantLib-Ruby
Version: 0.3.7
Epoch: 0
Release: 0
License: BSD License
Group: System Environment/Libraries
Packager: Liguo Song (Leo) <liguo.song@vanderbilt.edu>
Vendor: QuantLib.org
Source0: http://prdownloads.sourceforge.net/quantlib/QuantLib-Ruby-%{version}.tar.gz
URL: http://quantlib.org/
Buildroot: %{_tmppath}/%{name}-%{version}-root
AutoReq: no
Requires: QuantLib-devel == %{version}, ruby
BuildRequires: textutils, bash, ruby, ruby-libs, QuantLib-devel == %{version}

%description
QuantLib-Ruby is the Ruby wrapper for the QuantLib library.


%prep
%setup -q 

%if %(if [[ %{rbVMajor} -ge 1 ]]; then echo 1; else echo 0; fi) && %(if [[ %{rbVMinor} -ge 8 ]]; then echo 1; else echo 0; fi)
    %define test 1
%else
    %define test 0
%endif 


%build
%{ruby} setup.rb build
%if %{test}
  %{ruby} setup.rb test
%endif 


%install
rm -rf %{buildroot}
echo %{buildroot}/usr
%{ruby} setup.rb install --prefix=%{buildroot}/usr
mkdir %{buildroot}%{libdir}/ruby/QuantLib
mv %{buildroot}%{rb_libdir}/QuantLib.rb %{buildroot}%{libdir}/ruby/QuantLib/
mv %{buildroot}%{rb_archdir}/QuantLibc.so %{buildroot}%{libdir}/ruby/QuantLib/

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%(%{ruby} -e "require 'rbconfig';puts Config::CONFIG['libdir']")/ruby/QuantLib

%post
archdir=`%{ruby} -e "require 'rbconfig';puts Config::CONFIG['archdir']"`
rubylibdir=`%{ruby} -e "require 'rbconfig';puts Config::CONFIG['rubylibdir']"`
libdir=`%{ruby} -e "require 'rbconfig';puts Config::CONFIG['libdir']"`
ln -s $libdir/ruby/QuantLib/QuantLib.rb $rubylibdir/QuantLib.rb
ln -s $libdir/ruby/QuantLib/QuantLibc.so $archdir/QuantLibc.so

%postun 
rubylibdir=`%{ruby} -e "require 'rbconfig';puts Config::CONFIG['rubylibdir']"`
archdir=`%{ruby} -e "require 'rbconfig';puts Config::CONFIG['archdir']"`
rm -f $rubylibdir/QuantLib.rb $archdir/QuantLibc.so


%changelog
* Wed Nov 19 2003 Liguo Song <liguo.song@vanderbilt.edu>
- Update to 0.3.4

* Fri Oct 31 2003 Liguo Song <liguo.song@vanderbilt.edu>
- Initial QuantLib-Ruby package
