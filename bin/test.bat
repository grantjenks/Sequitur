@echo off

if "%1" == "" (
   echo No test cases specified.
   goto :EOF
)

pushd %~dp0

cd ..\tst

if exist .\log rd /S /Q .\log

md .\log
cd .\log

if not exist ..\%1 (
   echo Test cases not found.
   popd
   goto :EOF
)

for %%f in (..\%1\*.inp) do (
   echo Testing %%~nf

   ..\..\out\Sequitur.exe %%f > %%~nf.log
   diff ..\%1\%%~nf.out %%~nf.log
)

popd
