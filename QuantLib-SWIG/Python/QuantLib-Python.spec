Summary: The Python wrapper for the QuantLib library.
Name: QuantLib-Python
Version: 0.3.6
Epoch: 0
Release: 0
License: BSD License
Group: System Environment/Libraries
Packager: Liguo Song (Leo) <liguo.song@vanderbilt.edu>
Vendor: QuantLib.org
Source0: http://prdownloads.sourceforge.net/quantlib/QuantLib-Python-%{version}.tar.gz
URL: http://quantlib.org/
Buildroot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: QuantLib-devel == %{version}, python >= 2.1

%description
QuantLib-Python is the Python wrapper for the QuantLib library.


%prep
%setup -q 


%build
python setup.py build
python setup.py test


%install
rm -rf %{buildroot}
python setup.py install --prefix=%{buildroot}%{_prefix}

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%{_includedir}/QuantLib-Python
%{_libdir}/python2.2/site-packages/QuantLib


%changelog
* Wed Nov 19 2003 Liguo Song <liguo.song@vanderbilt.edu>
- Update to 0.3.4

* Thu Oct 30 2003 Liguo Song <liguo.song@vanderbilt.edu>
- Initial QuantLib-Python package
