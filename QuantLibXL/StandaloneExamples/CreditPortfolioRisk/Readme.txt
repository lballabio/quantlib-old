These sheets show part of the functionality of the latent models for credit risk and pricing on portfolios. 
Its better to recalculate them one by one or you will have to wait for quite a bit. You need to have the market definition ones open or linked; the others can be open one by one; they do not refer to each other.

 ----------- MARKET DEFINITION -------------
  <<01_Yield_01June2009.xls>> -----------------------
Contains the money curve and data set for the YTS, also sets the calculation date.

  <<TransitionData.xls>> ----------------------------
Contains a transition matrix per credit rating. It is intentionally simple not to load the calculations too much. It bootstraps default risk (real metric) curves per each of the ratings. Again for load reasons no term structure is used, just flat curves.

  <<02_ITRAXX_S9_01June2009Mkt.xls>> ----------------
Contains the CDS market snapshot for the iTraxx components on the calculation date, data points are not always real but approximate to the the real values. 

  <<03_ITRAXX_S9_CorrelMkt_01June2009.xls>> ---------
Contains the market for standard tranches on the iTraxx. Par prices are also expressed in base correlation.


 ----------- REPRICING THE MARKET ----------
Now the models are put to work in their pricing mode. Some of the calculations in the pricing are fairly heavy (a few minutes all models in one go) so your advised to edit the computations and play around with one model at a time.
 
  <<04a_RepriceItraxxTranchesBasic.xls>> ------------
Prices the correlation/tranche market with a number of more or less standard models. Compares models and pricing according to whether they deal with the basket heterogeneity or if they use a flat correlation or consider the correlation surface from the tranche market.
A model "performing good" would be a model offering a zero price surface (all tranches and all tenors) But since correlation is a price "performing good" would be matching the model the market uses to quote that correlation. 

  <<04b_RepriceItraxxTranchesExotic_RL.xls>> --------
Prices the tranche market using spot random recovery model in its simulation implementations. This models interest in its pricing mode lies in that it is be able to produce stressed market prices and provide non zero prices for tranches with loss levels over the market LGD loss level.


 ----------- DEFAULT RISK -------------------
  <<05_Riskmetrics.xls>> -----------
Models are used now to measure portfolio risk. Here we are in the real world and our metrics are from real observable statistics that we have probably taken in a leap of faith from some rating agency or internal model (see 'The plight of the fortune tellers' by R. Rebonato). So recoveries, probabilities and correlations are different and our risk is measured from those. This together with the previous pricing might offer a risk return relation. The factors used are random generated ones and the rating assignment in the 03_itraxx sheet is of my (arbitrary) making.
The main culprit for the long time the sheet takes to calculate is the saddle model (my saddle point search algorithm is pretty slow, its not the three dimensions). If you are in the multithread branch even the four MC in the sheet are very fast to compute. Of course its a small portfolio, I have tested it on thousands of counterparties; if you are to do that you have to be careful with memory (64 bits helps) since you will need to increase the number of simulations.







