Name:           orpie
Version:        1.4.0
Release:        1
Summary:        A fullscreen console-based RPN calculator application.
URL:            http://www.eecs.umich.edu/~pelzlpj/orpie/
Group:          Utilities/Math
License:        GPL
Source:         %{name}-%{version}.tar.gz
BuildRoot:      /var/tmp/%{name}-root
Requires:       ocaml >= 3.07, gsl >= 1.4, ncurses
BuildRequires:  ocaml, gsl-devel, ncurses-devel

%description
orpie is a fullscreen console-based RPN calculator that uses the curses
library.  Its operation is similar to that of modern HP calculators,
but data entry has been optimized for efficiency on a PC keyboard. Its
features include extensive scientific calculator functionality, command
completion, and a visible interactive stack.

%prep

%setup

%build

CFLAGS="$RPM_OPT_FLAGS" LDFLAGS=-s %{configure}
%{__make}

%install
[  %{buildroot} != "/" ] && rm -rf %{buildroot}


install -D -m 755 orpie-curses-keys.opt %{buildroot}%{_bindir}/orpie-curses-keys
install -D -m 755 orpie.opt             %{buildroot}%{_bindir}/orpie

install -D -m 644 orpierc               %{buildroot}%{_sysconfdir}/orpierc

install -D -m 644 doc/orpie.1              %{buildroot}%{_mandir}/man1/orpie.1
install -D -m 644 doc/orpie-curses-keys.1  %{buildroot}%{_mandir}/man1/orpie-curses-keys.1
install -D -m 644 doc/orpierc.5            %{buildroot}%{_mandir}/man5/orpierc.5

%clean
[  %{buildroot} != "/" ] && rm -rf %{buildroot}

%files
%defattr(-, root, root)
%{_bindir}/orpie-curses-keys
%{_bindir}/orpie
%{_mandir}/man1/orpie.1.gz
%{_mandir}/man1/orpie-curses-keys.1.gz
%{_mandir}/man5/orpierc.5.gz

%config %{_sysconfdir}/orpierc

%doc doc/manual.html doc/manual.pdf doc/manual.tex.in doc/TODO README COPYING ChangeLog

%changelog
* Mon Aug 2  2004 Chris Petersen <rpm@forevermore.net>
- Minor changes to spec format for better consistency and readability
* Tue Jun 15  2004 Chris Petersen <rpm@forevermore.net>
- Update RPM for 1.2rc1, and include orpie-curses-keys man info
* Tue Apr 6  2004 Chris Petersen <rpm@forevermore.net>
- Built initial RPM

