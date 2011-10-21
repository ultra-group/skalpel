;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2011 John Pirie
;; Copyright 2011 Heriot-Watt University
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;; Author: John Pirie
;; Description: Windows installer script to be compiled with the NSIS program
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


!define PRODUCT_NAME "Skalpel"
!define PRODUCT_VERSION "0.7"
!define PRODUCT_WEB_SITE "www.macs.hw.ac.uk/ultra/skalpel"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\skalpel-bin.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

; MUI 1.67 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall.ico"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\skalpel-bin.exe"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\Documentation\README"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "Setup.exe"
InstallDir "C:\Skalpel"
; InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

Section "MainSection" SEC01
  SetOutPath "$INSTDIR"
  SetOverwrite ifnewer
  File "..\..\..\Program Files\MLton\home\jpirie\tes\implementation\tes-seq\bin\skalpel-bin.exe"
  CreateDirectory "$SMPROGRAMS\Skalpel"
  CreateShortCut "$SMPROGRAMS\Skalpel\Skalpel.lnk" "$INSTDIR\skalpel-bin.exe"
  CreateShortCut "$DESKTOP\Skalpel.lnk" "$INSTDIR\skalpel-bin.exe"
  SetOutPath "$INSTDIR\lib"
  SetOverwrite try
  File "..\..\..\Program Files\MLton\home\jpirie\tes\implementation\lib\basis.sml"
  SetOutPath "$INSTDIR\EmacsUI"
  SetOverwrite try
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\.gitignore"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\skalpel-config.el"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\skalpel-debug-utils.el"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\SKALPEL-HELP"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\skalpel-main.el"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\ui\EmacsUI\skalpel-menu.el"
  SetOutPath "$INSTDIR\Documentation"
  SetOverwrite ifnewer
  File "..\..\..\Program Files\MLton\home\jpirie\tes\documentation\readme\README"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\package\shared\skalpel\CREDITS"
  File "..\..\..\Program Files\MLton\home\jpirie\tes\documentation\user-guide\user-guide.pdf"
SectionEnd

Section -AdditionalIcons
  SetOutPath $INSTDIR
  WriteIniStr "$INSTDIR\${PRODUCT_NAME}.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
  CreateShortCut "$SMPROGRAMS\Skalpel\Website.lnk" "$INSTDIR\${PRODUCT_NAME}.url"
  CreateShortCut "$SMPROGRAMS\Skalpel\Uninstall.lnk" "$INSTDIR\uninst.exe"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\skalpel-bin.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\skalpel-bin.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
SectionEnd


Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) was successfully removed from your computer."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove $(^Name) and all of its components?" IDYES +2
  Abort
FunctionEnd

; Essentially, this should be a complete list of every file that we distribute. We should check that this is the case before release.
Section Uninstall
  Delete "$INSTDIR\${PRODUCT_NAME}.url"
  Delete "$INSTDIR\uninst.exe"
  Delete "$INSTDIR\Documentation\user-guide.pdf"
  Delete "$INSTDIR\Documentation\CREDITS"
  Delete "$INSTDIR\Documentation\README"
  Delete "$INSTDIR\EmacsUI\skalpel-menu.el"
  Delete "$INSTDIR\EmacsUI\skalpel-main.el"
  Delete "$INSTDIR\EmacsUI\SKALPEL-HELP"
  Delete "$INSTDIR\EmacsUI\skalpel-debug-utils.el"
  Delete "$INSTDIR\EmacsUI\skalpel-config.el"
  Delete "$INSTDIR\EmacsUI\.gitignore"
  Delete "$INSTDIR\skalpel-bin.exe"

  Delete "$SMPROGRAMS\Skalpel\Uninstall.lnk"
  Delete "$SMPROGRAMS\Skalpel\Website.lnk"
  Delete "$DESKTOP\Skalpel.lnk"
  Delete "$SMPROGRAMS\Skalpel\Skalpel.lnk"

  RMDir "$SMPROGRAMS\Skalpel"
  RMDir "$INSTDIR\EmacsUI"
  RMDir "$INSTDIR\Documentation"
  RMDir "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd
