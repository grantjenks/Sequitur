@echo off

pushd %~dp0

cd ..

rd /S /Q .\out
rd /S /Q .\tst\log

popd
