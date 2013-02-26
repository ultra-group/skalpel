###############################################################
###############################################################
##
## Copyright 2009, 2010 Steven Shiells
## Copyright 2011 2013 Heriot Watt University
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
## Authors: Steven Shiells, John Pirie
## Date: December 2009
## Description: This is a shell script to run Skalpel, type 
##              error slicer for SML.
##
################################################################
################################################################

Name:    skalpel
Version: 0.8
Release: 1
Summary: A Type Error Slicer for the programming language SML
Group:   Development/Languages
License: GNUv3
URL:     http://www.macs.hw.ac.uk/ultra/skalpel
Source:  skalpel_0.8-src.tar.gz

Requires(post): info
Requires(preun): info

%post -n skalpel
/sbin/install-info /usr/local/share/info/Skalpel.info.gz /usr/share/info/dir || :

%preun -n skalpel
/sbin/install-info --delete /usr/local/share/info/Skalpel.info.gz /usr/share/info/dir || :
rm /usr/local/share/info/Skalpel.info.gz

%description
This is the source package for the Type Error Slicer, developed by the ULTRA group at the Heriot-Watt University.

%prep
tar -xf $RPM_SOURCE_DIR/%{name}_%{version}-src.tar.gz
rm -rf %{name}-%{version}/testing
cd %{name}-%{version}/analysis-engines/standard-ml/
autoconf
./configure --prefix=%{buildroot}/usr/local/

%build
cd %{name}-%{version}/analysis-engines/standard-ml/
make mlton-bin

%install
rm -rf %{buildroot}
cd %{name}-%{version}/analysis-engines/standard-ml/
mkdir -p %{buildroot}/usr/share/emacs/site-lisp/site-start.d/
mkdir -p %{buildroot}/usr/local/share/emacs/site-lisp/site-start.d/
make install-core # DESTDIR=%{buildroot}
cp ../../documentation/user-guide.pdf %{buildroot}/usr/local/share/skalpel/
gzip -9 ../../documentation/skalpel.1
mkdir -p %{buildroot}/usr/local/share/man/man1/
cp ../../documentation/skalpel.1.gz %{buildroot}/usr/local/share/man/man1/
gzip -9 ../../documentation/Skalpel.info
mkdir -p %{buildroot}/usr/local/share/info/
cp ../../documentation/Skalpel.info.gz %{buildroot}/usr/local/share/info/
cd %{buildroot}/../../
mkdir -p %{buildroot}/usr/local/share/emacs/site-lisp/skalpel-emacs
mkdir -p %{buildroot}/usr/local/share/emacs/site-start.d/
mkdir -p %{buildroot}/usr/share/emacs/site-lisp/site-start.d/
cp front-ends/emacs/skalpel-debug-utils.el %{buildroot}/usr/local/share/emacs/site-lisp/skalpel-emacs
cp front-ends/emacs/skalpel-main.el %{buildroot}/usr/local/share/emacs/site-lisp/skalpel-emacs
cp front-ends/emacs/skalpel-menu.el %{buildroot}/usr/local/share/emacs/site-lisp/skalpel-emacs
cp front-ends/emacs/skalpel-config.el %{buildroot}/usr/local/share/emacs/site-lisp/site-start.d/
cp front-ends/emacs/skalpel-config.el %{buildroot}/usr/share/emacs/site-lisp/site-start.d/

%clean
rm -rf %{buildroot}

%files
/usr/local/bin/skalpel
/usr/local/share/info/Skalpel.info.gz
/usr/local/share/man/man1/skalpel.1.gz
/usr/local/share/skalpel/basis.sml
/usr/local/share/skalpel/user-guide.pdf


### SKALPEL-EMACS PACKAGE

%package -n skalpel-emacs
Summary: Emacs UI for a Type Error Slicer for the programming language SML
Group:    Development/Languages
Requires: %{name}%{_isa} = %{version}-%{release}

%description -n skalpel-emacs
Files to allow Emacs to be used as a user interface for Skalpel.

%files -n skalpel-emacs
/usr/local/share/emacs/site-lisp/site-start.d/skalpel-config.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-debug-utils.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-main.el
/usr/local/share/emacs/site-lisp/skalpel-emacs/skalpel-menu.el
/usr/share/emacs/site-lisp/site-start.d/skalpel-config.el
