# building the graph
java -Xmx8G -jar otp.jar --build /path/to/dir --basePath /home/ja/Dropbox/work/canada_access/a_mon --analyst

# for localhosting and testing
java -Xmx8G -jar otp.jar --build /path/to/dir --inMemory --analyst

# running a jython command
jython -J-Xmx8g -Dpython.path=otp.jar t1.py

# setting up a graph folder
mkdir graphs
cd graphs
mkdir g
cd ..
mv Graph.obj graphs/g/
