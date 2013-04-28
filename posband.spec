%define name posband
%define version 0.9.9a1
%define release 1
%define prefix /usr
%define bindir %{prefix}/games
%define datadir %{prefix}/share/games/posband
%define statedir /var/lib/games/posband
%define installgroup games

Summary: A variant of Angband, the popular roguelike game
Name: %{name}
Version: %{version}
Release: %{release}
Copyright: Moria license
Group: Amusements/Games
Source0: http://posband.psd-solutions.com/downloads/src/posband-0.9.9a1-src.rar
URL: http://posband.psd-solutions.com
BuildRoot: %{_tmppath}/%{name}-root

%description
PosBand is a variant of Angband, one of the most popular
roguelike games (and successor to Moria). PosBand features
monster races, possession, themed level, infinite random
artifacts and many others.

%prep
%setup -q

%build
./configure --prefix=%{prefix} --bindir=%{bindir} --with-setgid=%{installgroup} \
	--with-libpath=%{datadir} --with-gcu --with-xaw --with-vcs CFLAGS="$RPM_OPT_FLAGS"
make

%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

make install DESTDIR="$RPM_BUILD_ROOT"

# Move the read-write data to /var/lib, link back to /usr/share/games
mkdir -p $RPM_BUILD_ROOT%{statedir}
for subdir in apex bone data save user ; do
	mv $RPM_BUILD_ROOT%{datadir}/$subdir $RPM_BUILD_ROOT%{statedir}
	ln -sf %{statedir}/$subdir $RPM_BUILD_ROOT%{datadir}
done

# Strip the executable
strip -s $RPM_BUILD_ROOT%{bindir}/posband

# Compress the manual page
gzip -9 $RPM_BUILD_ROOT%{prefix}/man/man6/posband.6

%clean
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

%files 
%doc COPYING.txt ChangeLog.txt README.txt TODO.txt FRONTEND.txt
%{bindir}/posband
%{datadir}
%{statedir}

%changelog

* Thu Jan 06 2005 Alexander Ulyanov <uav@urmail.ru>
- First PosBand RPM.

