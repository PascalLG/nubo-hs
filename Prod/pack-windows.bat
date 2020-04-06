mkdir tmp
copy /Y ..\Client\.stack-work\install\b3233136\bin\nubo-exe.exe tmp\nubo.exe
copy /Y c:\sqlite\sqlite3.dll tmp\sqlite3.dll
candle nubo.wxs
light nubo.wixobj
del nubo.wixobj
del nubo.wixpdb
rmdir /S /Q tmp
