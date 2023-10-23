# -*- coding: utf-8 -*-

import sys
from curvaIV_L_chromaXY_interface import Ui_MainWindow #used as generated in Qt Designer
import pyqtgraph as pg
from pyqtgraph.Qt import QtWidgets as qtw
import seabreeze as sb
from seabreeze.spectrometers import Spectrometer
import pyvisa as visa
import konica_minolta as km
import keithley236 as k236
import serial
from serial.tools import list_ports
import numpy as np
import time
import keyboard

class Measurement(qtw.QMainWindow, Ui_MainWindow):
    '''
      Reviewed 10th October, 2023
      ---------------------------
      Integrate the instruments:
      - Keithley 236 source meter           - GPIB interface
      - Konica Minolta CS-110 chromameter   - RS232 interface
      - Ocean Optics spectrometer           - USB interface
      
       Features:
          - dark counts correction
          - intensity artifact correction
          - freedom to use each instrument independently
    '''
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setObjectName("IV_Luminance_Chromaticity_Spectrum")
        self.setupUi(self)
        
        #Default values 
        self.lineEdit.setText("COM4")
        self.time_lineEdit.setText("5")
        self.timestp_lineEdit.setText("1")
        self.vstart_lineEdit.setText("0")
        self.vstop_lineEdit.setText("5")
        self.vstep_lineEdit.setText("1")
        self.compliance_lineEdit.setText("0.1")
        self.delay_lineEdit.setText("0.5")
        self.inttime_lineEdit.setText("6")
        self.apply_V_lineEdit.setText("0")
        
        #Link buttons to the correspondent actions
        self.startup_pushButton.clicked.connect(self.start_up)
        self.measure_pushButton.clicked.connect(self.measure)
        self.save_pushButton.clicked.connect(self.save)
        self.clear_pushButton.clicked.connect(self.clear)
        self.reset_pushButton.clicked.connect(self.reset)
        self.pushButton.clicked.connect(self.exit)
        self.apply_V_pushButton.clicked.connect(self.voltage)

    def graph_start(self):
        #Create empty arrays to initialize the plots
        self.t_array = []
        self.v_array = []
        self.curr_array = []
        self.l_data_array = []
        self.x_data_array = []
        self.y_data_array = []
        self.spec_array = [0]*2023
        #Initialize IV curve plot elements
        self.iv_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.iv_graphicsView.setLabel("left", "Current", units="A")
        #Initialize Luminance plot elements
        self.l_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.l_graphicsView.setLabel("left", "Luminance", units="Cd")
        #Initialize Chromaticity plot elements
        self.xy_graphicsView.setBackground(background=None)
        self.xy_graphicsView.setXRange(0, 0.8, padding=0)
        self.xy_graphicsView.setYRange(0, 0.8, padding=0)
        self.xy_graphicsView.setLabel("left", "Y", units="")
        self.xy_graphicsView.setLabel("bottom", "X", units="")
        #Initialize Spectra plot elements
        self.spec_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.spec_graphicsView.setLabel("left", "Intensity", units="A.U.")
        self.spec_graphicsView.setLabel("bottom", "Wavelength", units="nm")
        #Initialize plot values
        self.iv_dataset = self.iv_graphicsView.plot(self.v_array, self.curr_array)
        self.l_dataset = self.l_graphicsView.plot(self.v_array, self.l_data_array)          
        self.xy_dataset = self.xy_graphicsView.plot(self.x_data_array, self.y_data_array)

    def com_ports(self):
        #List all the serial ports 
        ports = serial.tools.list_ports.comports()
        #if no serial port is present
        if ports == []:
            com = ports        #define com as empty
        #Else if there is a list of serial ports
        else:
            for i in range(len(ports)):
                ports[i] = str(ports[i])                    #convert each port[i] to string
                if 'Prolific' in ports[i]:                  #if a Prolific type usb-serial converter 
                    raw_com = ports[i]                      #is being used to conect Konica Minolta
                    com = raw_com[:4]                       #make it the COM to be initialized
                    self.konicaport_label.setText(raw_com)  #show it in the interface label
                else:
                    raw_com = ports[i]                      #to any other type usb-serial converter
                    com = raw_com[:4]                       #make it the COM to be initialized
                    self.konicaport_label.setText(raw_com)  #show it in the interface label
        return com        

    def start_up(self):
        #Initialize Keithley source meter unit
        self.keithley = k236.Keithley236()          #instantiate keithley236 class
        gpib_p = self.keithley.gpib_set_up()        #initialize gpib interface
        if gpib_p == "not connected":               #if gpib is not connected
            self.keithstatus_label.setText("Keithley 236: not connected")   #show it in the interface label
            pass
        else:
            self.keithleyport_label.setText(gpib_p)                 #if gpib is connected
            self.keithstatus_label.setText("Keithley 236:    OK")   #print OK in the interface label
            self.keithley.start_up('Volts - dc', '4 readings', 'LineCycle (60Hz)')  #initialize measurement parameters
        #Initialize Konica Minolta chromameter
        self.chroma = km.KonicaMinolta()            #instantiate konica_minolta class
        self.com_p = self.com_ports()               #initialize rs232 interface
        if self.com_p == []:                        #if there are no rs232 instruments
            self.konicastatus_label.setText("Konica Minolta: not connected")    #show it in the interface label
            pass
        else:                                           #if there is a rs232 connected
            self.lineEdit.setText(self.com_p)           #print it in the interface label
            self.chroma.rs232_set_up(self.com_p)        #make it the COM port to connect
            chroma_start_status = self.chroma.start_up('PRESET', 'ABS.', 'FAST')            #initialize measurement parameters
            self.konicastatus_label.setText("Konica Minolta:    " + chroma_start_status)    #print the initialized status
        #Initialize Ocean Optics spectrometer
        self.ocean_list = sb.spectrometers.list_devices()   #Look for spectrometers connected
        if self.ocean_list == []:                           #if there are not spectrometers present
            self.oceanstatus_label.setText("Ocean Optics: not connected")   #show it in the interface label
            pass
        else:           #if there is a spectrometer present
            self.oceanport_label.setText(str(self.ocean_list))      #convert it to string        
            self.spec = Spectrometer.from_first_available()         #initialize it
            self.oceanstatus_label.setText("Ocean Optics:    OK")   #show OK status in the interface label

    def voltage(self):                          #apply voltage action
        v = self.apply_V_lineEdit.text()        #read voltage v to be applied
        self.keithley.run(v, 0, 0.1)            #apply it
    
    def spectrum(self):                                     #take a spectrum action
        inttime = int(self.inttime_lineEdit.text()) * 1000  #read integration time in ms and convert to microsec.
        self.spec.integration_time_micros(inttime)          #set spectrometer integration time
        raw_x = self.spec.wavelengths()                     #measure wavelength array
        raw_y = self.spec.intensities(correct_dark_counts = True, correct_nonlinearity = False) #measure intensities array
        x = np.delete(raw_x, [i for i in range(25)])        #delete the 25 first elements
        y = np.delete(raw_y, [i for i in range(25)])        #delete the 25 first elements
        self.spec_graphicsView.plot(x, y, clear=False)      #plot intensities x wavelength
        return x, y

    def lumin_xy(self):                                     #take luminance and chromaticity action
        lxy_data = self.chroma.measure()                    #measure luminance and chromaticity
        lxy_data_fmtd = self.chroma.format_data(lxy_data)   #format data 
        return lxy_data_fmtd
    
    def measure(self):                                      #measurement section
        self.graph_start()                                  #initialize the graph
        
        compliance = float(self.compliance_lineEdit.text()) #read the keithley compliance current to apply
        vstart = float(self.vstart_lineEdit.text())         #read start voltage
        vstep = float(self.vstep_lineEdit.text())           #read step voltage
        vstop = float(self.vstop_lineEdit.text())           #read stop voltage
        delay = float(self.delay_lineEdit.text())           #read the keithley delay to apply
        #if Vstart = Vstop --> constant voltage and varying time
        meas_time = float(self.time_lineEdit.text())        #read the time to measure (when Vstart = Vstop)
        tstep = float(self.timestp_lineEdit.text())         #read the time step (when Vstart = Vstop)
        #when the measurement is performed at constant V along the time    
        length_t = int(meas_time / tstep)                   #number of time steps to perform
        
        if vstop == vstart:             #condition to V be constant
            t = 0                       #initialize variable t
            self.iv_graphicsView.setLabel("bottom", "Time", units="sec")    #set labels to the IxV graph
            self.l_graphicsView.setLabel("bottom", "Time", units="sec")     #set labels to the Luminance graph
            for i in range(length_t + 1):                                   #set interval of measurement
                if keyboard.is_pressed('Escape'):                           #stop measurement by pressing ESC key
                    break
                current = self.keithley.run(vstart, delay, compliance)      #measure current with (parameters)
                self.curr_array = np.append(self.curr_array, current)       #append measured current to the current array
                self.t_array = np.append(self.t_array, t)                   #append corresponding t to the time array
                self.iv_dataset.setData(self.t_array, self.curr_array, pen=None, symbol='x')    #plot current x time point

                if self.com_p != []:
                    chroma_data = self.lumin_xy()                           #measure luminance and chromaticity      
                    self.l_data_array = np.append(self.l_data_array, [chroma_data[0]])  #append luminance to the lum. array
                    self.x_data_array = np.append(self.x_data_array, [chroma_data[1]])  #append x chromat. to the x array
                    self.y_data_array = np.append(self.y_data_array, [chroma_data[2]])  #append y chromat. to the y array
            
                    self.l_dataset.setData(self.t_array, self.l_data_array, pen=None, symbol='o')  #plot luminance x time point   
                    self.xy_dataset.setData(self.x_data_array, self.y_data_array, pen=None, symbol='+')  #plot chromaticity x time point
                else:
                    pass
                    
                if self.ocean_list != []:                                       
                    spec = self.spectrum()                                      #measure fluorescence spectrum
                    self.spec_graphicsView.plot(spec[0], spec[1], clear=False)  #plot fluorescence spectrum
                    if t == 0:
                        self.spec_array = (spec)                                #take the first spectrum
                    else:                                                       
                        self.spec_array = np.vstack((self.spec_array, spec[1])) #append each intensity column 
                else:
                    pass
                        
                pg.Qt.QtWidgets.QApplication.processEvents()    #guarantee showing each plot point as it is measured

                time.sleep(tstep)                               #wait time step to measure next point
                t += tstep                                      #increment t to build time array
            #build data array to be saved
            if self.com_p == []:                             #if Konica Minolta is not present    
                data_array = self.t_array, self.curr_array
                self.data = np.vstack(data_array)
            else:                                            #if it is
                data_array = self.t_array, self.curr_array, self.l_data_array, self.x_data_array, self.y_data_array
                self.data = np.vstack(data_array)
        #when the measurement is performed varying voltage
        else:
            n = 0                                                           #initialize counting 
            v = vstart                                                      #initialize variable v
            length_v = int((vstop - vstart)/vstep)                          #define the number of steps to perform
            self.iv_graphicsView.setLabel("bottom", "Voltage", units="V")   #set labels to the IxV graph
            self.l_graphicsView.setLabel("bottom", "Voltage", units="V")    #set labels to the LxV graph
            for i in range(length_v + 1):                                   #measurement section (the same steps as before)
                if keyboard.is_pressed('Escape'):                           #differing in just that it is measured I x V
                    break                                                   #and not I x t, as before          
                current = self.keithley.run(v, delay, compliance)
                self.curr_array = np.append(self.curr_array, current)
                self.v_array = np.append(self.v_array, v)
                self.iv_dataset.setData(self.v_array, self.curr_array, pen=None, symbol='x')

                if self.com_p != []:
                    chroma_data = self.lumin_xy()            
                    self.l_data_array = np.append(self.l_data_array, [chroma_data[0]])
                    self.x_data_array = np.append(self.x_data_array, [chroma_data[1]])
                    self.y_data_array = np.append(self.y_data_array, [chroma_data[2]])
            
                    self.l_dataset.setData(self.v_array, self.l_data_array, pen=None, symbol='o')          
                    self.xy_dataset.setData(self.x_data_array, self.y_data_array, pen=None, symbol='+')
                else:
                    pass

                if self.ocean_list != []:
                    spec = self.spectrum()
                    self.spec_graphicsView.plot(spec[0], spec[1], clear=False)
                    if v == vstart:
                        self.spec_array = (spec)
                    else:
                        self.spec_array = np.vstack((self.spec_array, spec[1]))
                        n += 1
                else:
                    pass
                        
                pg.Qt.QtWidgets.QApplication.processEvents()

                v += vstep

            if self.com_p == []:
                data_array = self.v_array, self.curr_array
                self.data = np.vstack(data_array)
            else:    
                data_array = self.v_array, self.curr_array, self.l_data_array, self.x_data_array, self.y_data_array
                self.data = np.vstack(data_array)

        self.reset()

        if self.ocean_list == []:
            return self.data
        else:
            return self.data, self.spec_array

    def save(self):                                         #save section
        self.data = self.data.transpose()                   #transpose data to save as columns      
        file_data = qtw.QFileDialog.getSaveFileName()[0]    #open a save dialog pop-up
        np.savetxt(file_data, self.data)                    #save data file in the specified folder
        if self.ocean_list != []:                           #if Ocean Optics is present
            self.spec_array = self.spec_array.transpose()   #transpose spectra data
            file_spec = qtw.QFileDialog.getSaveFileName()[0]#open a save dialog pop-up
            np.savetxt(file_spec, self.spec_array)          #save spectra file in the specified folder
        else:
            pass

    def clear(self):        #clear the graphics
        self.spec_graphicsView.clear()
        self.iv_graphicsView.clear()
        self.l_graphicsView.clear()
        self.xy_graphicsView.clear()

    def exit(self):         #close graphical interface
        self.close()

    def reset(self):        #reset Keithley
        self.keithley.reset()    

if __name__ == '__main__':
    app = qtw.QApplication([])
    tela = Measurement()
    tela.show()
    app.exec_()
