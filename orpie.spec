%define pkg_name        orpie
%define pkg_ver         1.1
%define pkg_rel         1
%define pkg_copy        GPL

Summary:        A fullscreen console-based RPN calculator application.
Source:         %{pkg_name}-%{pkg_ver}.tar.gz
URL:            http://www.eecs.umich.edu/~pelzlpj/orpie/
Group:          Utilities/Math
Name:           %{pkg_name}
Version:        %{pkg_ver}
Release:        %atrelease %{pkg_rel}
Copyright:      %{pkg_copy}
BuildRoot:      /var/tmp/%{pkg_name}-root
Requires:       ocaml, gsl, ncurses
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


install -D -m 755 orpie-curses-keys.opt   %{buildroot}%{_bindir}/orpie-curses-keys
install -D -m 755 orpie.opt               %{buildroot}%{_bindir}/orpie

install -D -m 644 orpierc                 %{buildroot}%{_sysconfdir}/orpierc

install -D -m 644 doc/orpie.1             %{buildroot}%{_mandir}/man1/orpie.1
install -D -m 644 doc/orpie-curses-keys.1 %{buildroot}%{_mandir}/man1/orpie-curses-keys.1
install -D -m 644 doc/orpierc.5           %{buildroot}%{_mandir}/man5/orpierc.5

%clean
[  %{buildroot} != "/" ] && rm -rf %{buildroot}

%files
%defattr(-, root, root)
/usr/bin/orpie-curses-keys
/usr/bin/orpie
/usr/share/man/man1/orpie.1.gz
/usr/share/man/man1/orpie-curses-keys.1.gz
/usr/share/man/man5/orpierc.5.gz

%config /etc/orpierc

%doc doc/manual.html doc/manual.pdf doc/manual.tex.in doc/TODO README COPYING ChangeLog

%changelog
* Tue Apr 6  2004 Chris Petersen <rpm@forevermore.net>
- Built initial RPM

