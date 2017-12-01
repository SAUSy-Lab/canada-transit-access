# function for computing travel time matrix
# set to loop over multiple departure times into a cube!

# run with ...
# jython -J-Xmx8g -Dpython.path=otp.jar matrix_otp_run.py

from org.opentripplanner.scripting.api import *
from org.opentripplanner.analyst.batch import BatchProcessor
from org.opentripplanner.scripting.api import OtpsEntryPoint
import time
import csv
from operator import itemgetter

print "cow!" # print cow!

# time for time
start_time = time.time()

csv_origins = "da_wpg_2016_2.csv"
csv_destinations = "ct_wpg_2016_2.csv"


# initial load graph
otp = OtpsEntryPoint.fromArgs([ "--graphs", "graphs", "--router", "g"])

router = otp.getRouter()

# basic routing stuff - can set more via json
r = otp.createRequest()



# function for single OD matrix
def OD_matrix(month,day,hour,minute):

    print "start" + "_" + str(day) + "_" + str(hour) + "_" + str(minute)

    r.setDateTime(2016, month, day, hour, minute, 00)
    r.setModes('WALK, TRANSIT')
    r.setMaxTimeSec(5400)
    r.setClampInitialWait(0)
    r.setMaxWalkDistance(999999999999999)

    # out lists
    out_table = []
    origin_list = []
    destination_list = []
    # grab lists of origina and destinations
    with open(csv_origins, 'rb') as csv_da:
        da_reader = csv.DictReader(csv_da)
        for da in da_reader:
            if str(da['dauid']) not in origin_list:
                origin_list.append(str(da['dauid']))
            with open(csv_destinations, 'rb') as csv_TAZ:
                TAZ_reader = csv.DictReader(csv_TAZ)
                for row in TAZ_reader:
                    if str(row['ctuid']) not in destination_list:
                        destination_list.append(str(row['ctuid']))

    # loop over input da and use as origins
    with open(csv_origins, 'rb') as csv_da:
        da_reader = csv.DictReader(csv_da)
        d = 0
        for da in da_reader:
            # add origin
            d_lat = float(da['Y'])
            d_lon = float(da['X'])
            # set up router for point
            r.setOrigin(d_lat, d_lon)
            spt = router.plan(r)
            # loop over input supers and use as destinations
            with open(csv_destinations, 'rb') as csv_super:
                super_reader = csv.DictReader(csv_super)
                s = 0
                for row in super_reader:
                    out_row = []
                    # add dest
                    lat = float(row['Y'])
                    lon = float(row['X'])
                    # solve
                    result = spt.eval(lat,lon)
                    if result is not None:
                        # get time
                        travel_time = result.getTime()
                        # output row to array
                        out_row = [d,str(da['dauid']),s,str(row['ctuid']),int(travel_time)]
                        out_table.append(out_row)
                    s += 1

            d += 1
            print '---------'
            print d
            print time.time() - start_time
            print '---------'

    # sort out_table
    #out_table = sorted(out_table, key=itemgetter(0,1))

    # grab list of all origins and destinations - can bring in externally if needed
    #origin_list = sorted(origin_list)
    #destination_list = sorted(destination_list)

    # set up matrix
    matrix = [[-1 for x in range(len(destination_list) + 1)] for y in range(len(origin_list) + 1)]

    print "------------"

    # code in matrix values
    for tr in out_table:
        matrix[tr[0]+1][tr[2]+1] = tr[4] # for rank id starting from 0
        # if rank id starts from 1, remove the +1 above

    # add row and column names
    x = 0
    while x < len(destination_list):
        x += 1
        matrix[0][x] = destination_list[x-1]
    x = 0
    while x < len(origin_list):
        x += 1
        matrix[x][0] = origin_list[x-1]

    matrix[0][0] = hour * 100 + minute

    # out to csv
    file_name = "out/t_" + "7_" + str(day) + "_" + str(hour) + "_" + str(minute) + ".csv"
    with open(file_name, 'wb') as csv_walk:
        writer = csv.writer(csv_walk)
        for tt in matrix:
            writer.writerow(tt)


# compute cube for every minute in an hour - here I guess it could be paralleled


m = 1
while m < 20:
    OD_matrix(5,11,8,m)
    m += 1
