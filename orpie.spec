Name:           orpie
Version:        1.5.0
Release:        1
Summary:        A fullscreen console-based RPN calculator application.

Group:          Utilities/Math
License:        GPL
URL:            http://pessimization.com/software/orpie/
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires:  ocaml
BuildRequires:  gsl-devel
BuildRequires:  ncurses-devel
Requires:       ocaml >= 3.07
Requires:       gsl >= 1.4
Requires:       ncurses

%description
orpie is a fullscreen console-based RPN calculator that uses the curses
library.  Its operation is similar to that of modern HP calculators,
but data entry has been optimized for efficiency on a PC keyboard. Its
features include extensive scientific calculator functionality, command
completion, and a visible interactive stack.

%prep
%setup -q


%build
%configure
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT

%check || :
#make test
#make check

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-, root, root)
%config %{_sysconfdir}/orpierc
%doc doc/manual.html doc/manual.pdf doc/manual.tex.in doc/TODO README COPYING ChangeLog
%{_bindir}/*
%{_mandir}/man[^3]/*


%changelog
* Mon Mar 21  2005 Chris Petersen <rpm@forevermore.net>
- Update spec to match fedora guidelines
* Mon Aug 2  2004 Chris Petersen <rpm@forevermore.net>
- Minor changes to spec format for better consistency and readability
* Tue Jun 15  2004 Chris Petersen <rpm@forevermore.net>
- Update RPM for 1.2rc1, and include orpie-curses-keys man info
* Tue Apr 6  2004 Chris Petersen <rpm@forevermore.net>
- Built initial RPM

