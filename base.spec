Summary: Angband/64 Angband variant / Roguelike.
Name: angband64-beta
Version: __version__
Release: __release__
Copyright: see lib/help/version.txt
Group: Games
%description
This is a variant on Angband-2.8.3. For more information, please go to
http://www.xs4all.nl/~thunder7 and read the documentation, which is also
in the rpm archive. For more information on Angband, please see the
official Angband homepage at http://www.phial.com or read on usenet:
rec.games.roguelike.angband
%prep
%build
%install
%post
test -d /usr/local/games/ang64/lib || echo "You should install the libraries (a64lb<version>.zip) too!"
echo "Wiping old data-files, if any"
rm -rf /usr/local/games/ang64/lib/data/*
%files
