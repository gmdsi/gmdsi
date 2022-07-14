rmdir /s distrib
mkdir distrib
cd distrib
copy ..\*.f
copy ..\*.f90
copy ..\*.bat
copy ..\*.exe
mkdir example
cd example
copy ..\..\example\*.*
cd ..

mkdir lumprep_example
cd lumprep_example
copy ..\..\lumprep_example\*.*
cd ..

mkdir documentation
cd documentation
copy c:\doc\rechmod\lumprem.pdf
cd ..
wzzip -rP lumprem.zip *.*