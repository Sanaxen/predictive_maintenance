# predictive_maintenance  
<div align="center">
<img src="./md/images/logo.png" width="80%">
</div>

---

# Battery Research Data  
<div align="center"> 
<img src="./dataset/BatteryLife/image.png">  
</div>  
https://calce.umd.edu/data#CS2  

'Discharge Capacity (mAh)' -> 'capacity'  
'Internal Resistance (Ohm)' -> 'resistance'  
'Constant Current Charging Time (s)' -> 'CCCT'  
'Constant Voltage Charging Time (s) -> 'CVCT'


<img src="./dataset/BatteryLife.png">  
<img src="./dataset/BatteryLife/image.gif">  
- Time stamp items added as appropriate  

### remaining useful life (RUL)
<img src="./dataset/BatteryLife/CS2_37_RUL000486.png">  
  
 --- 
  
# NASA Turbofan Jet Engine Data Set  
<div align="center"> 
<img src="./dataset/Turbofan_Jet_Engine.png">  
</div>  
https://www.kaggle.com/datasets/behrad3d/nasa-cmaps  



## NASA_Turbofan_Jet_Engine_Data_train_FD001_Uint1  
<img src="./dataset/Turbofan_Jet_Engine_s7.png">  
<img src="./dataset/Turbofan_Jet_Engine/result.gif">  
- Time stamp items added as appropriate  


### remaining useful life (RUL)
<img src="./md/images/NASA_Turbofan_Jet_Engine_Data_train_FD001_Uint1_RUL.png" width="100%">  


## NASA_Turbofan_Jet_Engine_Data_train_FD004_Uint1  
<img src="./dataset/Turbofan_Jet_Engine2_s7.png">  
<img src="./dataset/Turbofan_Jet_Engine2/result.gif">  
- Time stamp items added as appropriate  

### remaining useful life (RUL)
<img src="./md/images/NASA_Turbofan_Jet_Engine_Data_train_FD004_Unit1_RUL.png" width="100%">  
  
---

# WindTurbineHighSpeedBearingPrognosis-Data  
https://github.com/mathworks/WindTurbineHighSpeedBearingPrognosis-Data  
https://www.kaggle.com/datasets/luishpinto/wind-turbine-high-speed-bearing-prognosis-data  

(https://github.com/Sanaxen/predictive_maintenance/releases/download/untagged-905f728bcfa3c90b7849/WindTurbineHighSpeedBearingPrognosis-Data-main.zip)

<div align="center">
<img src="./dataset/dataset-card.jpeg" width="60%">
</div>  
<img src="./dataset/vibration.png">  
<img src="./md/images/result.gif">  
- Time stamp items added as appropriate

### remaining useful life (RUL)
<img src="./md/images/01st-day-vibration-2013_03_07 01_57_46.000_RUL.png" width="100%">  
<img src="./md/images/rul.gif" >

---
## requirements  

- [R-4.2.3](https://www.r-project.org/)
- [rtools](https://cran.r-project.org/bin/windows/Rtools/)
- [nkf](https://github.com/kkato233/nkf/releases )  
  using nkf to encode the character code.  
- 

## setup procedure
Install R(rtools) Modify the following files.

First line of init.bat (set drive=**c:**)
If you installed to drive D, set drive=**D:**
Make the same modification to tmp/init.bat

**install.bat** will automatically configure the necessary settings and make it available for use.  

See **md/predictive_maintenance_en.pdf** for details.  

---
Notes.  
The plot of the probability density function is fairly approximate.  
The confidence intervals for the predictions are tentatively calculated and not yet correct.  

Parts such as creating file lists from directories, processing files, and using batch startup are OS-dependent (Windows).  
To run on non-Windows operating systems, OS-dependent parts must be modified.  

