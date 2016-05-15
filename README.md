---
### Implementation of the Longstaff-Schwartz term structure model
---

**This was my master thesis. The GitHub version is currently under development. Please check back later!**

I explored the **Longstaff - Schwartz two-factor general equilibrium term structure model** [Longstaff - Schwartz, 1992] in my master thesis. 
This GitHub page is a collection of what I accomplished and found. I wanted to make this public in order to help fellow researchers.
The project is implemented in **R**.

In my thesis I present two calibration technique, the long-run and the short-run (market) calibration.
  
  
#### Long-run calibration of the Longstaff-Schwartz model
---

The long-run calibration technique I present here can be used for multiple purposes.  
- One purpose is to calibrate the model in-line with long-run economical trends and movements. 
  This technique is especially helpful for out-of-sample simulations.
- The other valid usage of this method is to estimate starting parameters for the market / short-run calibration.
  These parameters increase the speed of convergence of the second calibration method.

![alt text](https://github.com/alfoldib/longstaff-schwartz_model/blob/master/plots/plotGMMfit.png "Plot about the GMM fit")

The calibration method uses Hansen's Generalized Method of Moments [Hansen, 1983].
  
  
#### Short-run (market) calibration of the Longstaff-Schwartz model
---

The short-run (market) calibration method enables the user to calibrate the model to a specific trading day.
This alorithm minimizes the difference between the observed market prices and the model fitted prices. 
The obtained parameters are the best estimate given the Longstaff-Schwartz model is correctly specified.
This technique enables the user to further investigate and price interest rate derivatives (like bond options, swaptions, etc).

<img src="https://plot.ly/~Alfoldib/155/hungarian-government-bond-zero-coupon-yield-curve/.png">
  
The calibration method uses a powerful differential evolution algorithm [Storn - Price, 1997] in optimizing, while searching for the optimal parameters.
  
  
**Contents:**
- [x] Database about the Hungarian Government bonds ranging from 2003 to 2015 
- [x] Scripts for cleaning and correcting the raw data
- [ ] Script for estimating the state variables with GARCH
- [ ] Script for estimating long-run model parameters with GMM
- [x] Script for estimating short-run model parameters with DE optimization
- [x] Script for estimating short-run model parameters with DE optimization with initial population
- [x] Script for complete loop over the trading days between 2003 and 2015 to calculate the daily zero-coupon yield curve 