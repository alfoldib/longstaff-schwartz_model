---
### Implementation of the Longstaff-Schwartz term structure model
---


**This was my master thesis. The GitHub version is currently under development. Please check back later!**


I explored the `Longstaff - Schwartz two-factor general equilibrium term structure model` in my master thesis. 
This GitHub page is a collection of what I accomplished, found. I wanted to make this public in order to help fellow researchers.
The project is implemented in `R`.

In my thesis I present two calibration technique:
1) Long-run calibration
2) Market calibrtion

**Contents:**
- [x] Database about the Hungarian Government bonds from 2003 to 2015 
- [x] Scripts for cleaning and correcting the raw data
- [ ] Script for estimating the state variables with GARCH
- [ ] Script for estimating long-run model parameters with GMM
- [x] Script for estimating short-run model parameters with DE optimization
- [x] Script for estimating short-run model parameters with DE optimization with initial population
- [x] Script for complete loop over the trading days between 2003 and 2015 to calculate the daily zero-coupon yield curve 

---
#### Long-run calibration of the Longstaff-Schwartz model
---

The long-run calibration technique I present can be used for multiple purposes. 
One purpose is to calibrate the model in-line with long-run economical trends and movements. 
The other valid usage is to estimate parameters for a market / short-run calibration.

![alt text](https://github.com/alfoldib/longstaff-schwartz_model/blob/master/plots/plotGMMfit.png "Plot about the GMM fit")

---
#### Calibration of the Longstaff-Schwartz model to observed market prices
---

<iframe src="https://plot.ly/~Alfoldib/155/hungarian-government-bond-zero-coupon-yield-curve/.embed"
        height="600" width="100%"
        scrolling="no" seamless="seamless"
        frameBorder="0">
</iframe>



