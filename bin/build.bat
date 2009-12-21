@echo off

if "%1" == "" (
   echo No build type specified.
   goto :EOF
)

pushd %~dp0

cd ..

if not exist .\out md .\out

cd .\out

if not exist ..\bld\%1.rsp (
   echo Build type not recognized.
   popd
   goto :EOF
)

del .\*.rsp
del .\*.cpp

copy ..\bld\%1.rsp .\
copy ..\src\Sequitur.cpp .\

cl @%1.rsp

if not %errorlevel% == 0 (
   echo Error building Sequitur.
   popd
   goto :EOF
) else (
   echo Sequitur built successfully.
)

popd
