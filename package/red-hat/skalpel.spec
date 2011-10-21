###############################################################
###############################################################
##
## Copyright 2009, 2010 Steven Shiells
## Copyright 2011 Heriot Watt University
##
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
##
##
## Authors: Steven Shiells
## Date: December 2009
## Description: This is a shell script to run Skalpel, type 
##              error slicer for SML.
##
################################################################
################################################################

Name:    skalpel
Version: 0.7
Release: 1
Summary: A Type Error Slicer for the programming language SML
Group:   Development/Languages
License: GNUv3
URL:     http://www.macs.hw.ac.uk/ultra/compositional-analysis/type-error-slicing/
Source:  skalpel_0.7-src.tar.gz

%description
This is the source package for the Type Error Slicer, developed by the ULTRA group at the Heriot-Watt University.

%prep
tar -xf $RPM_SOURCE_DIR/%{name}_%{version}-src.tar.gz
cd %{name}-%{version}
./configure

%build
cd %{name}-%{version}
make

%install
rm -rf %{buildroot}
cd %{name}-%{version}
mkdir -p %{buildroot}/usr/share/emacs/site-lisp/site-start.d/
make install DESTDIR=%{buildroot}

%clean
rm -rf %{buildroot}

%files
#%doc %{name}-%{version}/usr/local/doc/skalpel/README
/usr/local/bin/skalpel
/usr/local/bin/skalpel-bin
/usr/local/bin/skalpel-legend
/usr/local/bin/skalpel-perl-to-bash
/usr/local/doc/skalpel/README
#/usr/local/share/emacs/site-lisp/site-start.d/skalpel-config.el
#/usr/local/share/emacs/site-lisp/skalpel-emacs/sml-full-symbol-description.el
#/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-debug-utils.el
#/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-main.el
#/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-menu.el
#/usr/local/share/info/Skalpel.info.gz
/usr/local/share/man/man1/skalpel-bin.1.gz
/usr/local/share/man/man1/skalpel-legend.1.gz
/usr/local/share/man/man1/skalpel-perl-to-bash.1.gz
/usr/local/share/man/man1/skalpel.1.gz
#/usr/local/share/man/man1/skalpel-emacs.1.gz
/usr/local/share/skalpel/basis.sml
#/usr/local/share/skalpel-emacs/SKALPEL-HELP
/usr/local/share/skalpel/database/code*.sml
/usr/local/share/skalpel/database/test*.sml
/usr/local/share/skalpel/database/output*.html
#/usr/share/emacs/site-lisp/site-start.d/skalpel-config.el


### SKALPEL-EMACS PACKAGE

%package -n skalpel-emacs
Summary:  Emacs UI for a Type Error Slicer for the programming language SML
Group:    Development/Languages
Requires: %{name}%{_isa} = %{version}-%{release}

Requires(post): info
Requires(preun): info

%description -n skalpel-emacs
This is the Emacs UI for skalpel

%post -n skalpel-emacs
/sbin/install-info /usr/local/share/info/Skalpel.info.gz /usr/share/info/dir || :

%preun -n skalpel-emacs
/sbin/install-info --delete /usr/local/share/info/Skalpel.info.gz /usr/share/info/dir || :
rm /usr/local/share/info/Skalpel.info.gz

%files -n skalpel-emacs
/usr/local/share/emacs/site-lisp/site-start.d/skalpel-config.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-debug-utils.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-main.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-menu.el
/usr/local/share/info/Skalpel.info.gz
/usr/local/share/man/man1/skalpel-emacs.1.gz
/usr/local/share/skalpel-emacs/SKALPEL-HELP
/usr/share/emacs/site-lisp/site-start.d/skalpel-config.el
