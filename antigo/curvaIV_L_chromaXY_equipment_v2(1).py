# -*- coding: utf-8 -*-
# Integra os instrumentos:
#     - Keithley 236
#     - Konica Minolta CS-110
#     - Ocean Optics
# revisão 03/05/2023

import sys
from curvaIV_L_chromaXY_interface import Ui_MainWindow
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
       Update:
          - dark counts correction
          - intensity artifact correction
          - freedom to use each the instrument independently
    '''
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setObjectName("IV_Luminance_Chromaticity_Spectrum")
        self.setupUi(self)

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
        
        self.startup_pushButton.clicked.connect(self.start_up)
        self.measure_pushButton.clicked.connect(self.measure)
        self.save_pushButton.clicked.connect(self.save)
        self.clear_pushButton.clicked.connect(self.clear)
        self.reset_pushButton.clicked.connect(self.reset)
        self.pushButton.clicked.connect(self.exit)
        self.apply_V_pushButton.clicked.connect(self.voltage)

    def graph_start(self):
        self.t_array = []
        self.v_array = []
        self.curr_array = []
        self.l_data_array = []
        self.x_data_array = []
        self.y_data_array = []
        self.spec_array = [0]*2023
        
        self.iv_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.iv_graphicsView.setLabel("left", "Current", units="A")

        self.l_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.l_graphicsView.setLabel("left", "Luminance", units="Cd")

        self.xy_graphicsView.setBackground(background=None)
        self.xy_graphicsView.setXRange(0, 0.8, padding=0)
        self.xy_graphicsView.setYRange(0, 0.8, padding=0)
        self.xy_graphicsView.setLabel("left", "Y", units="")
        self.xy_graphicsView.setLabel("bottom", "X", units="")

        self.spec_graphicsView.showGrid(x=True, y=True, alpha=True)
        self.spec_graphicsView.setLabel("left", "Intensity", units="A.U.")
        self.spec_graphicsView.setLabel("bottom", "Wavelength", units="nm")

        self.iv_dataset = self.iv_graphicsView.plot(self.v_array, self.curr_array)
        self.l_dataset = self.l_graphicsView.plot(self.v_array, self.l_data_array)          
        self.xy_dataset = self.xy_graphicsView.plot(self.x_data_array, self.y_data_array)

    def com_ports(self):
        ports = serial.tools.list_ports.comports()
        if ports == []:
            com = ports
        else:
            for i in range(len(ports)):
                ports[i] = str(ports[i])
                if 'Prolific' in ports[i]:
                    raw_com = ports[i]
                    com = raw_com[:4]
                    self.konicaport_label.setText(raw_com)
        return com        

    def start_up(self):
        self.keithley = k236.Keithley236()
        gpib_p = self.keithley.gpib_set_up()
        if gpib_p == "not connected":
            self.keithstatus_label.setText("Keithley 236: not connected")
            pass
        else:
            self.keithleyport_label.setText(gpib_p)
            self.keithstatus_label.setText("Keithley 236:    OK")
            self.keithley.start_up('Volts - dc', '4 readings', 'LineCycle (60Hz)')

        self.chroma = km.KonicaMinolta()      
        self.com_p = self.com_ports()
        if self.com_p == []:
            self.konicastatus_label.setText("Konica Minolta: not connected")
            pass
        else:
            self.lineEdit.setText(self.com_p)
            self.chroma.rs232_set_up(self.com_p)            
            chroma_start_status = self.chroma.start_up('PRESET', 'ABS.', 'FAST')
            self.konicastatus_label.setText("Konica Minolta:    " + chroma_start_status)

        self.ocean_list = sb.spectrometers.list_devices()
        if self.ocean_list == []:            
            self.oceanstatus_label.setText("Ocean Optics: not connected")
            pass
        else:
            self.oceanport_label.setText(str(self.ocean_list))        
            self.spec = Spectrometer.from_first_available()
            self.oceanstatus_label.setText("Ocean Optics:    OK")

    def voltage(self):
        v = self.apply_V_lineEdit.text()
        self.keithley.run(v, 0, 0.1) 
    
    def spectrum(self):
        inttime = int(self.inttime_lineEdit.text()) * 1000        
        self.spec.integration_time_micros(inttime)
        raw_x = self.spec.wavelengths()
        raw_y = self.spec.intensities(correct_dark_counts = True, correct_nonlinearity = False)
        x = np.delete(raw_x, [i for i in range(25)])
        y = np.delete(raw_y, [i for i in range(25)])
        self.spec_graphicsView.plot(x, y, clear=False)
        return x, y

    def lumin_xy(self):
        lxy_data = self.chroma.measure()
        lxy_data_fmtd = self.chroma.format_data(lxy_data)
        return lxy_data_fmtd
    
    def measure(self):
        self.graph_start()
        
        compliance = float(self.compliance_lineEdit.text())
        vstart = float(self.vstart_lineEdit.text())
        vstep = float(self.vstep_lineEdit.text())
        vstop = float(self.vstop_lineEdit.text())
        delay = float(self.delay_lineEdit.text())
        meas_time = float(self.time_lineEdit.text())
        tstep = float(self.timestp_lineEdit.text())
                
        length_t = int(meas_time / tstep)

        if vstop == vstart:
            t = 0
            self.iv_graphicsView.setLabel("bottom", "Time", units="sec")
            self.l_graphicsView.setLabel("bottom", "Time", units="sec")
            for i in range(length_t + 1):
                if keyboard.is_pressed('Escape'):
                    break
                current = self.keithley.run(vstart, delay, compliance)
                self.curr_array = np.append(self.curr_array, current)
                self.t_array = np.append(self.t_array, t)
                self.iv_dataset.setData(self.t_array, self.curr_array, pen=None, symbol='x')

                if self.com_p != []:
                    chroma_data = self.lumin_xy()            
                    self.l_data_array = np.append(self.l_data_array, [chroma_data[0]])
                    self.x_data_array = np.append(self.x_data_array, [chroma_data[1]])
                    self.y_data_array = np.append(self.y_data_array, [chroma_data[2]])
            
                    self.l_dataset.setData(self.t_array, self.l_data_array, pen=None, symbol='o')          
                    self.xy_dataset.setData(self.x_data_array, self.y_data_array, pen=None, symbol='+')
                else:
                    pass
                    
                if self.ocean_list != []:
                    spec = self.spectrum()
                    self.spec_graphicsView.plot(spec[0], spec[1], clear=False)
                    if t == 0:
                        self.spec_array = (spec)
                    else:
                        self.spec_array = np.vstack((self.spec_array, spec[1]))
                else:
                    pass
                        
                pg.Qt.QtWidgets.QApplication.processEvents()

                time.sleep(tstep)
                t += tstep
                
            if self.com_p == []:
                data_array = self.t_array, self.curr_array
                self.data = np.vstack(data_array)
            else:
                data_array = self.t_array, self.curr_array, self.l_data_array, self.x_data_array, self.y_data_array
                self.data = np.vstack(data_array)
                
        else:
            n = 0
            v = vstart
            length_v = int((vstop - vstart)/vstep)
            self.iv_graphicsView.setLabel("bottom", "Voltage", units="V")
            self.l_graphicsView.setLabel("bottom", "Voltage", units="V")
            for i in range(length_v + 1):
                if keyboard.is_pressed('Escape'):
                    break
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

    def save(self):        
        self.data = self.data.transpose()          
        file_data = qtw.QFileDialog.getSaveFileName()[0]
        np.savetxt(file_data, self.data)
        if self.ocean_list != []:
            self.spec_array = self.spec_array.transpose()
            file_spec = qtw.QFileDialog.getSaveFileName()[0]
            np.savetxt(file_spec, self.spec_array)
        else:
            pass

    def clear(self):
        self.spec_graphicsView.clear()
        self.iv_graphicsView.clear()
        self.l_graphicsView.clear()
        self.xy_graphicsView.clear()

    def exit(self):
        self.close()

    def reset(self):
        self.keithley.reset()    

if __name__ == '__main__':
    app = qtw.QApplication([])
    tela = Measurement()
    tela.show()
    app.exec_()
