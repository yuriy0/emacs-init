:: Emacs startup script
@echo off

:: Set the path to where the Emacs binaries are
set binpath=C:\Program Files\emacs\bin

if "%~1"=="" (
  "%binpath%\emacsclientw.exe" --no-wait --alternate-editor="" --eval "(make-frame-visible)"
) else (
  "%binpath%\emacsclientw.exe" --no-wait --alternate-editor="" "%~1"
)
