---
### Implementation of the Longstaff-Schwartz term structure model
---


**This was my master thesis. The GitHub version is currently under development. Please check back later!**


I explored the `Longstaff - Schwartz two-factor general equilibrium term structure model` in my master thesis. 
This GitHub page is a collection of what I accomplished, found. I wanted to make this public in order to help fellow researchers.
The project is implemented in `R`.

**Contents:**
- [x] Database about the Hungarian Government bonds from 2003 to 2015 
- [x] Scripts for cleaning and correcting the raw data
- [ ] Script for estimating the state variables with GARCH
- [ ] Script for estimating long-run model parameters with GMM
- [x] Script for estimating short-run model parameters with DE optimization
- [x] Script for estimating short-run model parameters with DE optimization with initial population
- [x] Script for complete loop over the trading days between 2003 and 2015 to calculate the daily zero-coupon yield curve 

---
#### Long run calibration of the Longstaff-Schwartz model
---

![alt text](https://github.com/alfoldib/longstaff-schwartz_model/blob/master/plots/plotGMMfit.png "Plot about the GMM fit")

---
#### Calibration of the Longstaff-Schwartz model to observed market prices
---

![alt text](https://github.com/alfoldib/longstaff-schwartz_model/blob/master/plots/hunGov_zero-coupon_yield.png "Plot hungarian government bond zero curve")