# -*- coding: utf-8 -*-
# revisão 14/03/2023

import serial
import time

class KonicaMinolta():
    '''
    Command list
    ------------

    MES
    MDS
    CLE
    RCR
    TDR
    UCR
    TDS
    TDW
    UCW

    Usage
    -----
    import konica_minolta as km
    import time
    
    chroma = km.KonicaMinolta()
    chroma.rs232_set_up()
    chroma.start_up('PRESET', 'ABS.', 'FAST')
    data = chroma.measure()
    s = chroma.format_data(data)
    print('luminance = ', float(s[0]))
    print('chromaticityX = ', float(s[1]))
    print('chromaticityY = ', float(s[2]))         
    '''

    error_check_code = dict([
        ('OK00', 'Normal operation'),
        ('OK11', 'Chromaticity measuring range over'),     #same as ----E0 display
        ('OK12', 'Luminance display range over'),          #same as E9 display
        ('OK13', 'Luminance display range under'),         #same as flickering display
        ('ER00', 'Command error'),                         #command out of the parameter setting range
        ('ER01', 'Setting error'),                         #same as E display
        ('ER11', 'Memory value error'),                    #same as E1 display
        ('ER10', 'Luminance and Chromaticity measuring range over'),     #same as E0 display
        ('ER12', 'Luminance display range, chromaticity display range simultaneous over'),
        ('ER20', 'EEPROM error'),                          #same as E2 display
        ('ER30', 'Battery out')                
        ])

    calibration = dict([
        ('PRESET', 'MDS,00\r\n'),      
        ('VARI.','MDS,01\r\n')       
    ])

    meas_mode = dict([
        ('ABS.', 'MDS,04\r\n'),      
        ('DIFF.','MDS,05\r\n')       
    ])

    response = dict([
        ('FAST', 'MDS,06\r\n'),      
        ('SLOW','MDS,07\r\n')       
    ])    
    
    def __init__(self):
        self.brand = 'Konica Minolta'
        self.model = 'CS-100A'
        
    def rs232_set_up(self, com_port):
        self.ser = serial.Serial(timeout=10)
        self.ser.port = com_port		
        self.ser.baudrate = 4800
        self.ser.bytesize = serial.SEVENBITS
        self.ser.parity = serial.PARITY_EVEN        
        self.ser.stopbits = serial.STOPBITS_TWO
        self.ser.open()
                
    def start_up(self, cal, color_mode, resp):
        cal_bytes = bytes(self.calibration[cal], 'utf-8')        
        self.ser.write(cal_bytes)
        time.sleep(0.1)
        cal_setng = self.ser.read(6)
        cal_setng = cal_setng.decode()
        cal_setng = cal_setng[:4]
                
        color_mode_bytes = bytes(self.meas_mode[color_mode], 'utf-8')
        self.ser.write(color_mode_bytes)
        time.sleep(0.1)
        meas_setng = self.ser.read(6)
        meas_setng = meas_setng.decode()
        meas_setng = meas_setng[:4]
        
        resp_bytes = bytes(self.response[resp], 'utf-8')
        self.ser.write(resp_bytes)
        time.sleep(0.1)
        resp_setng = self.ser.read(6)
        resp_setng = resp_setng.decode()
        resp_setng = resp_setng[:4]
        
        if cal_setng == 'OK00' and meas_setng == 'OK00' and resp_setng == 'OK00':
            return 'OK'
        else:
            return cal_setng, cal_setng, cal_setng
                            
    def measure(self):
        self.ser.write(b'MES\r\n')
        time.sleep(0.1)
        data = self.ser.read(28)       
        return data       
    
    def format_data(self, raw_data):
        data = raw_data.split()        
        luminance = data[1].decode()
        L = float(luminance[:(len(luminance) - 1)])        
        x_chromatcty = data[2].decode()
        X = float(x_chromatcty[:(len(x_chromatcty) - 1)])
        y_chromatcty = data[3].decode()
        Y = float(y_chromatcty[:(len(y_chromatcty))])
        return L, X, Y

    def rs232_close(self):
        self.ser.close() 
        
        
    
