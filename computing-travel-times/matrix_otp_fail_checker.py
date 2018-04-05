# # finds the set of points which do not snap to graph nodes,
# and then outputs them to a csv for vis checking in qgis

# jython -Dpython.path=otp.jar otp_fail_checker.py

# jython -J-Xmx8g -Dpython.path=otp.jar otp_fail_checker.py





from org.opentripplanner.scripting.api import *
import time
import csv

in_name = "da_wpg_2016_2.csv"
out_name = "fail_da.csv"


# time script
start_time = time.time()

# initial load graph - have graphs store in "graphs" folder
otp = OtpsEntryPoint.fromArgs([ "--graphs", "graphs", "--router", "g"])

# set up routing
router = otp.getRouter()
r = otp.createRequest()
r.setDateTime(2017, 4, 10, 0, 0, 00) # departure date / time
r.setModes('WALK') # modes to include
r.setMaxTimeSec(9999999999999999) # time out (in seconds)
r.setClampInitialWait(0) # include any waiting time it output


# print time for fun!
print time.time() - start_time




# input list of destinations - can query if needed
dest_table = []
o = []
with open(in_name, "r") as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        # here we could query the destinations
        dest = [row['X'],row['Y']]
        if len(o) == 0:
            o.append(float(row['X']))
            o.append(float(row['Y']))
            print o
        dest_table.append(dest)

# loop over each row, computing travel times to dests

out_stats = []

out_fail_table = []


r.setOrigin(o[1], o[0]) # set origin
spt = router.plan(r)

out_travel_times = []
print time.time() - start_time

for row in dest_table:

    #print float(row[1]),float(row[0])
    result = spt.eval(float(row[1]),float(row[0])) # set destination
    if result is not None:
        travel_time = result.getTime()
        out_travel_times.append(travel_time)
    else:
        out_fail = [float(row[1]),float(row[0])]
        out_fail_table.append(out_fail)


# output csv name
with open(out_name, 'w') as csvw:
    writer = csv.writer(csvw)
    writer.writerow(['Y','X'])
    for oo in out_fail_table:
        writer.writerow(oo)

# print computation time - again for fun!
print ("----------------")
print time.time() - start_time

# mention how much fun that was!
print "that was fun!"
