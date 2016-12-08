#!/usr/bin/env python


######################################
# basic file reader parser
# for dynap-se caer files
# author federico.corradi@inilabs.com
# co-author V. Tettamanti - valerite@student.ethz.ch
######################################


import socket
import struct
import numpy as np
import time
import matplotlib
from matplotlib import pyplot as plt
import os
from matplotlib.backends.backend_pdf import PdfPages
import pylab



# you will need to change this!!
#filename = '/Users/Vale/Documents/AAA SCUOLA/ETH/First Project/DataSpike/caerOut04-2016_11_23_14_43_15.aedat'
#file_read = open(filename, "rb")


debug = False



def skip_header():
    ''' This function skip the standard header of the recording file '''
    line = file_read.readline()
    while line.startswith("#"):
        if (line == '#!END-HEADER\r\n'):
            break
        else:
            line = file_read.readline()


def read_events():
    """ A simple function that read dynap-se events from cAER aedat 3.0 file format"""

    # raise Exception at end of file
    data = file_read.read(28)
    if (len(data) <= 0):
        print("\033[1;38m read all data\033[1;m \n")
        raise NameError('END OF DATA')

    # read header
    eventtype = struct.unpack('H', data[0:2])[0]
    eventsource = struct.unpack('H', data[2:4])[0]
    eventsize = struct.unpack('I', data[4:8])[0]
    eventoffset = struct.unpack('I', data[8:12])[0]
    eventtsoverflow = struct.unpack('I', data[12:16])[0]
    eventcapacity = struct.unpack('I', data[16:20])[0]
    eventnumber = struct.unpack('I', data[20:24])[0]
    eventvalid = struct.unpack('I', data[24:28])[0]
    next_read = eventcapacity * eventsize  # we now read the full packet
    data = file_read.read(next_read)
    counter = 0  # eventnumber[0]
    # spike events
    core_id_tot = []
    chip_id_tot = []
    neuron_id_tot = []
    ts_tot = []
    # special events
    spec_type_tot = []
    spec_ts_tot = []

    if (eventtype == 0):
        spec_type_tot = []
        spec_ts_tot = []
        while (data[counter:counter + eventsize]):  # loop over all event packets
            special_data = struct.unpack('I', data[counter:counter + 4])[0]
            timestamp = struct.unpack('I', data[counter + 4:counter + 8])[0]
            spec_type = (special_data >> 1) & 0x0000007F
            spec_type_tot.append(spec_type)
            spec_ts_tot.append(timestamp)
            if (spec_type == 6 or spec_type == 7 or spec_type == 9 or spec_type == 10):
                print (timestamp, spec_type)
            counter = counter + eventsize
    elif (eventtype == 12):
        while (data[counter:counter + eventsize]):  # loop over all event packets
            aer_data = struct.unpack('I', data[counter:counter + 4])[0]
            timestamp = struct.unpack('I', data[counter + 4:counter + 8])[0]
            core_id = (aer_data >> 1) & 0x0000001F
            chip_id = (aer_data >> 6) & 0x0000003F
            neuron_id = (aer_data >> 12) & 0x000FFFFF
            core_id_tot.append(core_id)
            chip_id_tot.append(chip_id)
            neuron_id_tot.append(neuron_id)
            ts_tot.append(timestamp)
            counter = counter + eventsize
            if (debug):
                print("chip id " + str(chip_id) + '\n')
                print("core_id " + str(core_id) + '\n')
                print("neuron_id " + str(neuron_id) + '\n')
                print("timestamp " + str(timestamp) + '\n')
                print("####\n")

    return core_id_tot, chip_id_tot, neuron_id_tot, ts_tot, spec_type_tot, spec_ts_tot


if __name__ == '__main__':



    # plt.figure(figsize=(8.125,11.556), dpi=1200)
    # plt.rc('xtick', labelsize=5)
    # plt.rc('ytick', labelsize=5)
    # plt.subplots_adjust(hspace=0.5, top=0.95, bottom=0.05)


    # open more than one .aedat files in order to automatize the process
    folder = '/Users/Vale/Documents/AAA SCUOLA/ETH/First Project/data.analysis/multiple_chip/data.spike/changedc/1'  # your path for the .aedatfiles
    for f in os.listdir(folder):
        if f.endswith('.aedat'):
            print("\nFile to convert:\n")
            print('\033[1;36m'+f+'\033[1;m\n')
            filename = f
            file_read = open(folder+"/"+f, "rb")

            done_reading = False

            # skip comment header of file
            skip_header()

            # prepare lists
            core_id_tot = []
            chip_id_tot = []
            neuron_id_tot = []
            ts_tot = []
            # special events
            spec_type_tot = []
            spec_ts_tot = []

            while (done_reading == False):
                try:
                    core_id, chip_id, neuron_id, ts, spec_type, spec_ts = read_events()
                    core_id_tot.extend(np.array(core_id))
                    chip_id_tot.extend(np.array(chip_id))
                    neuron_id_tot.extend(np.array(neuron_id))
                    ts_tot.extend(np.array(ts))
                    spec_type_tot.extend(np.array(spec_type))
                    spec_ts_tot.extend(np.array(spec_ts))
                except NameError:
                    file_read.close()
                    done_reading = True

            # make all arrays
            core_id_tot = np.array(core_id_tot)
            chip_id_tot = np.array(chip_id_tot)
            neuron_id_tot = np.array(neuron_id_tot)
            ts_tot = np.array(ts_tot)



            # save arrays as .txt file as float

            np.savetxt(filename + ".txt", np.c_[core_id_tot, chip_id_tot, neuron_id_tot, ts_tot], fmt='%f', header="core_id\tchip_id\tneuron_id\ttime", comments='')

            # For saving single files

            # np.savetxt(filename + "_core_id.txt", core_id_tot, fmt='%f')
            # np.savetxt(filename + "_chip_id.txt", chip_id_tot, fmt='%f')
            # np.savetxt(filename + "_neuro_id.txt", neuron_id_tot, fmt='%f')
            # np.savetxt(filename + "_ts.txt", ts_tot, fmt='%f')

            # get the index for spikes coming from different cores
            # we have only mapped a single chip in output, chip id 4.
            # we do not care about chip_id
            indx_core_zero = np.where(np.transpose(core_id_tot) == 0)[0]
            indx_core_one = np.where(np.transpose(core_id_tot) == 1)[0]
            indx_core_two = np.where(np.transpose(core_id_tot) == 2)[0]
            indx_core_three = np.where(np.transpose(core_id_tot) == 3)[0]


            # informations, works only if there are no txt files present in the folder
            folder1 = '/Users/Vale/PycharmProjects/analyze.aedat'
            num_files = len([g for g in os.listdir(folder1) if g.endswith('.txt')])
            num_data = len([g for g in os.listdir(folder) if g.endswith('.aedat')])

            print 'File used is:', num_files
            print 'Number of total files still to process:', num_data - num_files, '\n'


            print "//////////////////////////////////////\n"



    print('\033[1;35m Done!\033[1;m')



