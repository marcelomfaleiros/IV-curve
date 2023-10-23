#encoding: utf-8
# Reviewed 10yh October, 2023

import pyvisa as visa
import time

class Keithley236():
    
    '''
    Controla o eletrômetro Keithley 617 via porta GPIB.

    Voltage source range: ±100uV to ±110V
    Current compliance: up to 100mA

    Current source range: ±100fA to ±100mA
    Voltage compliance: up to 110V

    Voltage meter range: ±10uV to ±110V
    Current meter range: ±10fA to ±100mA

    Measurement consists of a series of source-delay-measure (SDM) cycles:
    1. Set the source output level
    2. Perform the delay
    3. Make the measurement
    
    ----------------------------------------------------------------------------------------------------------------------------------
    Mode            |           Command           |              Description           |                Parameters
    ----------------------------------------------------------------------------------------------------------------------------------
    Modify sweep    | A(level),(range),(delay),   | Modify sweep list points           | level - I-source (-100.00 to +100.00) mA
                    |                             |                                    |         V-source (-110.00 to +110.00) V
                    | first(,last)                |                                    | range
                    |                             |                                    | delay - (0 - 65000) milliseconds
                    |                             |                                    | first - (1 - 1000)
                    |                             |                                    | last - (1 - 1000)
    ---------------------------------------------------------------------------------------------------------------------------------- 
    Bias operation  | B(level),(range),(delay)    | Select bias operation              | level - I-source (-100.00 to +100.00) mA
                    |                             |                                    |         V-source (-110.00 to +110.00) V
                    |                             |                                    | range
                    |                             |                                    | delay - (0 - 65000) milliseconds
    ----------------------------------------------------------------------------------------------------------------------------------
    Calibration     | C step(,value)X             | Calibrate instrument               |
    ----------------------------------------------------------------------------------------------------------------------------------  
    Display         | D0X                         | Return display to normal           |
                    | D1,aaa...aX                 | Display ASCII characters (18 max.) |
                    | D2,aaa...aX                 | Display and store ASCII characters |
                    |                             | (18 max.)                          |
    ----------------------------------------------------------------------------------------------------------------------------------
    Source and      | F(source),(function)        | Select source (V or I) and         | F0,0 - source V, dc
    function        |                             | function     (dc or sweep)         | F0,1 - source V, sweep
                    |                             |                                    | F1,0 - source I, dc
                    |                             |                                    | F1,1 - source I, sweep
    ----------------------------------------------------------------------------------------------------------------------------------
    Output data     | G(items),(format),(lines)   | Select items included, format, and | items
    format          |                             | lines per talk in output           | format
                    |                             |                                    | lines
    ----------------------------------------------------------------------------------------------------------------------------------
    IEEE immediate  | H0X                         | Cause an immediate bus trigger     | 
    trigger         |                             |                                    | 
    ---------------------------------------------------------------------------------------------------------------------------------- 
    Self-tests      | J0X                         | Restor factory dafaults            | 
                    | J1X                         | Perform memory test                | 
                    | J2X                         | Perform display test               |    
    ----------------------------------------------------------------------------------------------------------------------------------
    EOI and bus     | K0                          | Enable EOI, enable hold-off on X   | 
    hold-off        | K1                          | Disable EOI, enable hold-off on X  | 
                    | K2                          | Enable EOI, disable hold-off on X  | 
                    | K3                          | Disable EOI, disable hold-off on X |                  
    ----------------------------------------------------------------------------------------------------------------------------------
    Compliance      | L(level),(range)            | Set compliance level and range     | level
                    |                             |                                    | range                    
    ----------------------------------------------------------------------------------------------------------------------------------
    SRQ mask and    | M(mask),(compliance)        | Select conditions that will cause a| mask
    serial poll byte|                             | service-request                    | compliance - 0 or 1
    ----------------------------------------------------------------------------------------------------------------------------------
    Operate         | N0                          | Place unit in standby mode         | Standby
                    | N1                          | Place unit in operate mode         | Operate
    ----------------------------------------------------------------------------------------------------------------------------------
    Output sense    | O0                          | Select local sensing               | 
                    | O1                          | Select remote sensing              | 
    ---------------------------------------------------------------------------------------------------------------------------------- 
    Filter          | P0                          | Measurement filter disabled        | 
                    | P1                          | 2-reading filter                   | 
                    | P2                          | 4-reading filter                   | 
                    | P3                          | 8-reading filter                   | 
                    | P4                          | 16-reading filter                  | 
                    | P5                          | 32-reading filter                  | 
    ----------------------------------------------------------------------------------------------------------------------------------
    Create/append   | Q0,(level),(range),         | Create fixed level sweep           | count - (1 to 1000)
    sweep list      | (delay),(count)             |                                    | delay - (0 - 65000) milliseconds
                    | Q1,(start),(stop),(step),   | Create linear stair sweep          | level - I-source (-100.00 to +100.00) mA 
                    | (range),(delay)             |                                    |         V-source (-110.00 to +110.00) V
                    | Q2,(start),(stop),(points), | Create logarithmic stair sweep     | points - 0 = 5 points per decade
                    | (range),(delay)             |                                    |          1 = 10 points per decade
                    | Q3,(level),(range),         | Create fixed level pulsed sweep    |          2 = 25 points per decade
                    | (pulses),(ton),(toff)       |                                    |          3 = 50 points per decade
                    | Q4,(start),(stop),(step),   | Create linear stair pulsed sweep   | pulses - (0 - 500)
                    | (range),(ton),(toff)        |                                    | range 
                    | Q5,(start),(stop),          | Create logarithmic stair pulsed    | start, stop - I-source (-100.00 to +100.00) mA
                    | (points),(range),(ton),     | sweep                              |               V-source (-110.00 to +110.00) V
                    | (toff)                      |                                    | ton - (0 - 65000) milliseconds
                    | Q6,(level),(range),         | Append fixed level sweep           | toff - (0 - 65000) milliseconds
                    | (delay),(count)             |                                    | 
                    | Q7,(start),(stop),(step),   | Append linear stair sweep          | 
                    | (range),(delay)             |                                    | 
                    | Q8,(start),(stop),(points), | Append logarithmic stair sweep     | 
                    | (range),(delay)             |                                    | 
                    | Q9,(level),(range),         | Append fixed level pulsed sweep    | 
                    | (pulses),(ton),(toff)       |                                    | 
                    | Q10,(start),(stop),(step),  | Append linear stair pulsed sweep   | 
                    | (range),(ton),(toff)        |                                    | 
                    | Q11,(start),(stop),(points),| Append logarithmic stair pulsed    | 
                    | (range),(ton),(toff)        | sweep                              | 
    -----------------------------------------------------------------------------------------------------------------------------------  
    Trigger control | R0                          | Disable input/output triggers      | 
                    | R1                          | Enable input/output triggers       | 
    ----------------------------------------------------------------------------------------------------------------------------------- 
    Integration time| S0                          | 416usec integration time           | Fast - 4-digit resolution
                    | S1                          | 4msec integration time             | Medium - 5-digit resolution
                    | S2                          | 16.67msec integration time         | LineCycle (60Hz) - 5-digit resolution
                    | S3                          | 20msec integration time            | LineCycle (50Hz) - 5-digit resolution
    -----------------------------------------------------------------------------------------------------------------------------------
    Trigger         | T(origin),(in),(out),(end)  | Program input trigger origin and   | origin - origin of input triggers
    configuration   |                             | effects, output triggers, and end- | in - effect of an input trigger
                    |                             | of-sweep output trigger            | out - when an output trigger is generated                   
                    |                             |                                    | end - sweep End^ trigger out
    ----------------------------------------------------------------------------------------------------------------------------------- 
    Status          | U0                          | Send model no. and revision        | 
                    | U1                          | Send error status word             | 
                    | U2                          | Send stored ASCII string           |  
                    | U3                          | Send machine status word           | 
                    | U4                          | Send measurement parameters        | 
                    | U5                          | Send compliance value              | 
                    | U6                          | Send suppression value             | 
                    | U7                          | Send calibration status word       | 
                    | U8                          | Send defined sweep size            | 
                    | U9                          | Send warning status word           | 
                    | U10                         | Send first sweep pint in compliance| 
                    | U11                         | Send sweep measure size            | 
    -----------------------------------------------------------------------------------------------------------------------------------
    1100V range     | V0                          | Disable 1100V range                | 
    control         | V1                          | Enable 1100V range                 | 
    -----------------------------------------------------------------------------------------------------------------------------------
    Default delay   | W0                          | Disable dafault delay              | 
                    | W1                          | Enable default delay               | 
    ----------------------------------------------------------------------------------------------------------------------------------- 
    Execute         | X                           | Execute commands                   | 
    ----------------------------------------------------------------------------------------------------------------------------------- 
    Terminator      | Y0                          | <CR><LF>                           | 
                    | Y1                          | <LF><CR>                           | 
                    | Y2                          | <CR>                               | 
                    | Y3                          | <LF>                               | 
                    | Y4                          | None                               | 
    ----------------------------------------------------------------------------------------------------------------------------------- 
    Supress         | Z0                          | Disable suppression                | 
                    | Z1                          | Enable suppression                 | 
    ----------------------------------------------------------------------------------------------------------------------------------- 

    Update:
    -------

    - correction for returning '' if instrument is OFF;
    
    Usage:
    ------

    import keithley236 as kt
    k = kt.Keithley236()
    k.gpib_set_up()

    '''

    function = dict([
        ('Volts - dc', 'F0,0X'),
        ('Volts - sweep', 'F0,1X'),
        ('Amps - dc','F1,0X'),
        ('Amps - sweep', 'F1,1X')
    ])

    volts_range = dict([
        ('Auto', '0'),
        ('1.1V', '1'),
        ('11V', '2'),
        ('110V', '3'),
    ])
    
    amps_range = dict([
        ('Auto', '0'),
        ('1nA', '1'), 
        ('10nA', '2'), 
        ('100nA', '3'), 
        ('1uA', '4'), 
        ('10uA', '5'),        
        ('100uA', '6'),
        ('1mA', '7'),
        ('10mA', '8'),
        ('100mA', '9'),
        ('1A', '10')
    ])

    output_items = dict([
        ('No items', 'G0,'),
        ('Source value', 'G1,'),
        ('Delay value', 'G2,'),
        ('Measure value', 'G4,'),
        ('Time value', 'G8,')    
    ])

    output_format = dict([
        ('ASCII with prefix and suffix', '0,'),
        ('ASCII with prefix, no suffix', '1,'),
        ('ASCII, no prefix or suffix', '2,'),
        ('HP binary data', '3,'),
        ('IBM binary data', '4,')    
    ])

    output_lines = dict([
        ('One line of dc data per talk', '0X'),
        ('One line of sweep data per talk', '1X'),
        ('All lines of sweep data per talk', '2X')
    ])

    self_tests = dict([
        ('Restore factory defaults', 'J0X'),
        ('Perform memory test', 'J1X'),
        ('Perform display test', 'J2X')
    ])

    SRQ_mask = dict([
        ('SRQ disabled', '0'),
        ('Warning', '1'),
        ('Sweep done', '2'),
        ('Trigger out', '4'),
        ('Reading done', '8'),
        ('Ready for trigger', '16'),
        ('Error', '32'),
        ('Compliance', '128')
    ])

    filter_mode = dict([
        ('Disabled', 'P0X'),
        ('2 readings', 'P1X'),
        ('4 readings', 'P2X'),
        ('8 readings', 'P3X'),
        ('16 readings', 'P4X'),
        ('32 readings', 'P5X')        
    ])

    integration_time = dict([
        ('Fast', 'S0X'),
        ('Medium', 'S1X'),
        ('LineCycle (60Hz)', 'S2X'),
        ('LineCycle (50Hz)', 'S3X')               
    ])

    trigger_origin = dict([
        ('IEEE X', '0'),
        ('IEEE GET', '1' ),
        ('IEEE Talk', '2'),
        ('External (TRIGGER IN pulse)', '3'),
        ('Immediate only (front panel MANUAL key or H0X command', '4') 
    ])

    trigger_in = dict([
        ('Continuous', '0'),
        ('^SRC DLY MSR (trigger starts source phase)', '1'),
        ('SRC^DLY MSR (trigger starts delay phase)', '2'),
        ('^SRC^DLY MSR', '3'),
        ('SRC DLY^MSR (trigger starts measure phase)', '4'),
        ('^SRC DLY^MSR', '5'),
        ('SRC^DLY^MSR', '6'),
        ('^SRC^DLY^MSR', '7'),
        ('^Single pulse', '8')
    ])

    trigger_out = dict([
        ('None during sweep', '0'),
        ('SRC^DLY MSR', '1'),
        ('SRC DLY^MSR', '2'),
        ('SRC^DLY^MSR', '3'),
        ('SRC DLY MSR^', '4'),
        ('SRC^DLY MSR^', '5'),
        ('SRC DLY^MSR^', '6'),
        ('SRC^DLY^MSR^', '7'),
        ('Pulse end^', '8')
    ])

    trigger_end = dict([
        ('Disabled', '0'),
        ('Enabled', '1')
    ])

    def __init__(self):
        self.brand = 'Keithley'
        self.model = '236'
        
    def gpib_set_up(self):
        self.rm = visa.ResourceManager()
        resources = self.rm.list_resources()                #list visa resources
        if 'GPIB0::16' not in str(resources):               #if GPIB address 16 not present
                rsrcs = "not connected"                     #define "not connected" status
        else:
            for i in range(len(resources)):                             #search in the list
                if 'GPIB0::16' in resources[i]:                         #for GPIB address 16
                    rsrcs = resources[i]                                #name it
                    self.keithley236 = self.rm.open_resource(rsrcs)     #open gpib port
                    self.keithley236.timeout = 25000                    #set timeout
        return rsrcs
            
    def start_up(self, meter_mode, average, int_time):
        self.keithley236.write(self.function[meter_mode])       #set function                  
        self.keithley236.write(self.filter_mode[average])       #set filter                      
        self.keithley236.write(self.integration_time[int_time])
        output_info = self.output_items['Measure value'] + self.output_format['ASCII, no prefix or suffix'] + self.output_lines['One line of dc data per talk']                         
        self.keithley236.write(output_info)                     #output data format
        self.keithley236.write('O0X')                           #set local sense  
        self.keithley236.write('N1X')                           #operate mode
                
    def stand_by(self):
        self.keithley236.write('N0X')                           #standby mode

    def calibration(self):
        pass

    def reset(self):
        self.keithley236.write('B0,0,0X')                       #reset instrument
        
    def run(self, volts, delay, compliance):
        self.keithley236.write('L' + str(compliance) + ',0X')   #set compliance 
        bias_command = 'B' + str(volts) + ',0,' + str(delay)
        self.keithley236.write(bias_command)                    #set bias 
        y = float(self.keithley236.query('H0X'))                #measure current
        
        return y
