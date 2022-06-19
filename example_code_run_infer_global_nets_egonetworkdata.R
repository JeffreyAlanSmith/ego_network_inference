# This is example R code, showing how to take input ego network data
# and run the Ego Network Configuration (ENC) inferential
# introduced in: Smith, Jeffrey A. 2012. “Macrostructure 
# from Microstructure: Generating Whole Systems from 
# Ego Networks.” Sociological Methodology 42:155-205. 
# See also: Smith, Jeffrey A. and G. Robin Gauthier 2020. 
# “Estimating Contextual Effects from Ego Network Data”. 
# Sociological Methodology. 50:215-275 doi: 10.1177/0081175020922879.
#
# The basic idea is to take information from the ego network data,
# like the number of partners named and the pattern of alter-alter ties, 
# and generate networks that are consistent with the features found 
# in the sampled ego network data. The generated networks represent a 
# researcher’s best guess as to what the true network actually looks 
# like and can then be used to calculate key features of interest. 

# In this case we will demonstrate the basic approach on one network (i.e., one ego network
# sample coming from one network). This code will yield the inferred networks. A researcher 
# could then take those inferred networks and measure various statistics of interest,
# using those inferred statistics in subsequent analyses. 
# Note that this analysis will assume that there is no measurement error in the 
# recorded ego network data. The goal here is simply to demonstrate how to 
# take ego network data and use that to make inference about global network properties. 

# For this example code we will focus on a single network from the main analysis,
# where we used generated networks as the true, known network. We include ego network 
# data for the other example networks on the github page where this code is housed. 
# There is one .Rdata file for each network, representing an example ego network sample 
# drawn from the network in question. This code assumes that all functions and input
# data sets have been downloaded and saved and can be read into R. 

# We begin by setting up some parameters for the simulation and reading in our 
# example ego network data. We can read in our example ego network data directly 
# from the following url: 

url_ego <- "https://github.com/JeffreyAlanSmith/ego_network_inference/raw/main/data/example_ego_network_data.Rdata"

load(url(description = url_ego))

# Let's take a look at the ego network data we will be working with 
head(ego_network_data)

# We see demographic attributes for ego (race, educ_cat, gender)
# as well for the named alters, up to 5 (race1, race2...)
# There is a column called deg, which captures the degree of the node.

# Let's also check the number of rows:
nrow(ego_network_data)

# So, we have 300 sampled egos, with ego and alter attributes for 
# race, gender and education. We also have degree information for ego. 

# And now we will read in the associated alter-alter tie data:
url_alter_alter <- "https://github.com/JeffreyAlanSmith/ego_network_inference/raw/main/data/example_alter_alter_ties.Rdata"

load(url(description = url_alter_alter))

head(alter_alter_tie_data)

dim(alter_alter_tie_data)

# This data frame shows the ties between alters (up to the first five), assumed
# to be in the following order: 1-2, 1-3, 1-4, 1-5; 2-3, 2-4, 2-5; 3-4, 3-5; 4-5
# There are thus 10 columns. The 300 rows is the same as with the ego network data
# and corresponds to the sampled cases (the two data frames are
# assumed to be in the same order).

# Our goal is to take the information on the 300 cases (degree, ego/alter attributes, alter-alter
# ties) and try to infer what the full network looks like. 

# Here we set some useful inputs that we will use below: 

# name of degree column on dataset:
var.name.degree <- "deg"

# names of key ego attribute columns in data frame:
var.name.characs <- c("race", "educ_cat", "gender") 

# Here we are going to create a list describing the columns names of the alter
# attributes on the ego network data frame.
# The alter attribute columns have the form of race1 race2...
# We will create a list, where each slice is a different attribute, 
# first race then education, then gender.

var.name.characs.alter <- list()
for (p in 1:length(var.name.characs)){
var.name.characs.alter[[p]] <- paste(var.name.characs[[p]], 1:5, sep = "")
}

# Let's take a look:
var.name.characs.alter

# Let's also set some basic information about the ego network data and the 
# network to be generated: 

max.alter <- 5 # maximum number of alters we have information about
size.net <- 1500 # size of network to generate

##################################################
# Reading in functions

# Now we read in the core functions for the simulation approach. All functions 
# can be read into R using: 

source("https://github.com/JeffreyAlanSmith/ego_network_inference/raw/main/functions/source_ego_network_inference_functions.R")


# Now we load some useful packages. 
library(igraph)
library(gtools)
library(biglm)
library(doParallel)

# Note that the functions also need network and ergm. We read in those below.

##########################################
# A note on smoothing out the degree distribution
# Sometimes it can be useful to 'smooth out' the higher degree values found in the 
# sample, so that the few high degree nodes in the simulated network do not 
# have all exactly the same degree. This is not strictly necessary, and is up to the
# discretion of the researcher to decide how (or if) to adjust the degree distribution
# from what is found in the sampled data. We will not do this here although we do employ
# this strategy in the actual paper. Interested researchers should be careful
# that any smoothing does not add too many high degree nodes into the network, as we
# (probably) do not want to increase the standard deviation of the 
# degree distribution too far above what is found in the sample. 

#####################################################################
# Getting input into simulations

# We will now take the ego network data and construct some useful inputs that 
# will be used in the simulation itself. We will use the function prepare_ego_nets_for_sim. 
# The main inputs are the ego network data and the objects created above (like var.name.characs,
# max.alter and so on). 

# The inputs are: 
# data=input ego network data
# degree=vector of degree values for simulated network, or NULL (if NULl
# then the function uses the degree from the degree variable on the ego network 
# data frame)
# size.net=size of network to create
# var.name.degree=name of variable in data that has the degree of the respondents
# var.name.characs= vector of names of attributes for ego
# alter.tie.data=a data frame indicating if alter 1 is tied to 2, 1 to 3...
# max.alter=max number of alters the respondents were allowed to name
# resp.weights=vector of weights to create representative population  (or NULL)
# var.name.characs.alter=list of column names for alter attributes, same order as var.name.charac

set.seed(10002)

ego_dat <- prepare_ego_nets_for_sim(data = ego_network_data,
degree = NULL
,size.net = size.net
,var.name.degree = var.name.degree,
var.name.characs = var.name.characs
,alter.tie.data = alter_alter_tie_data,
max.alter = max.alter,
resp.weights = NULL
,var.name.characs.alter = var.name.characs.alter)

names(ego_dat)

# The outputted object is a list with a number of useful items. Most important
# are the initial network (where the simulation will begin), called initial.randnet
# and egodist, which is the distribution of ego network configuration based on the 
# sample. 

# Note that this starting network is a random network of the correct degree distribution
# with demographic attributes mapped onto the nodes in a way that is consistent
# with the sampled data. We could also use an alternative starting network constructed
# using a different approach. This can be useful if the network has 
# high clustering, and we want to start the simulations at a 
# network that has features closer to what we think the final network will have.

# Here we will start with the network outputted above.Let's go ahead and 
# grab initial.randnet. 

initial.randnet <- ego_dat$initial.randnet

# Now we will set the formula that will be used for the simulations.

formula_egosim <- as.formula(initial.randnet ~ nodecov("deg") + gwesp(.3, fixed = T)
+ nodemix("race") + nodemix("educ_cat") + nodematch("gender"))

# In this example the simulation is based on gwesp, mixing on race, education and gender.
# Note that this would depend on the variables available. Note also that 
# we specified slightly different models for some of the other networks used in the paper. 
# See Appendix F for details. Note that we set the left hand side of the equation 
# as the starting network of the simulation (defined above).  
# Also note that we set the gwesp term to fixed=T, meaning we hold the decay term fixed,
# although it is possible to allow that value to vary.
# The decay term's value is set at .3.

# As a second preliminary step, we will now set the initial values for the coefficients 
# used in the simulation. We will start with the homophily coefficients. The function is 
# estimate_initial_homophily_coefs and utilizes case control logistic regression,
# although it is possible to use other approaches. The main inputs are the formula,
# the ego network data frame and the vectors defining the variables on the data frame. 

# formula=formula that will be used within simulation, here used to 
# to get the desired homophily terms
# data=input ego network data
# var.name.degree=name of variable in data that has the degree of the respondents
# var.name.characs= vector of names of attributes for ego
# resp.weights=vector of weights to create representative population  (or NULL)
# var.name.characs.alter=list of column names for alter attributes, same order as var.name.charac
# max.control.data.N=max size used when constructing baseline random comparison
# in case control model. 

homoph.coefs <- estimate_initial_homophily_coefs(formula = formula_egosim,
data = ego_network_data
,var.name.degree = var.name.degree, 
var.name.characs = var.name.characs
,var.name.characs.alter = var.name.characs.alter,
resp.weights = NULL,
max.control.data.N = 100000)

# The output is a set of coefficients for the homophily terms in the model. 

homoph.coefs

# Now, we need to specify the remaining (initial) coefficients for the simulation. 
# We take the homophily coefficients from above, but also need to put in 
# values for the node covariate term and GWESP. We will set the nodecov coefficient to .6.
# For GWESP, we can either attempt to estimate an initial value or the starting point
# can come from past knowledge. Here we put in .5 as a first guess. Note that the coefficients
# must be in the right order to match the formula. Note also that we might choose
# different initial starting points for different networks. Also remember that we set
# the decay value for gwesp above (as .3).

initial.nonhomoph.coefs <- c(.6, .5)

coefs <- c(initial.nonhomoph.coefs, homoph.coefs)

###############################
# Running Simulation
# We are now in a position to actually run the simulation to infer
# complete networks from the sampled ego network data.
# The function is whole_network_function. Note there are many possible inputs and options.
# See the documentation in the actual code for details. 
# For our purposes here, the main inputs are:

# params=initial coefficients (here set to coefs)
# tableego=distribution of ego networks from sample (here set to ego_dat$egodist)
# formula=formula used in simulation (set above as formula_egosim)
# initial.network=network where simulation should start (here set to initial.randnet)
# constant=which coefficients are to be held constant throughout the updating process (here just
# the first one, corresponding to nodecov)
# pos.cols=which coefficients are allowed to be updated but are restricted to be positive (here
# the second column, pertaining to GWESP)
# constant.homophily.change=which coefficients are held constant as the algorithm 
# searches for a better fit for the ego network distribution 
# but are allowed to vary to ensure the right level of homophily (here all homophily terms)
# egonet.data=sampled ego network data
# var.name.degree=name of variable in egonet.data that has the degree of the respondents
# (set above as var.name.degree)
# var.name.characs= a vector of variable names in egonet.data corresponding to the homophily characteristics
# of the respondent (set above)
# var.name.characs.alter= a list, where each element is a vector 
# of variable names corresponding to the homophily characteristics of the alters (set above)

# There are also various inputs that we set to control how the algorithm runs, including:
# burnin, interval, delta, delta1, sample, ego.samp.size and constraints. 
# See notes in function for more details.
# There are also options to do parallel processing, set via useparallel and num.cores.
# Parallel processing can speed things up considerably. 
# Finally, break_ifnot_improved determines if the algorithm should stop if there is no
# expected improvement for updating the coefficients. If F, the algorithm
# will continue to run until it reaches max.iter.
# Here we utilize 4 CPUs and set max.iter to 3.
# Even with 4 CPUs this can take a couple of minutes to run (~ 2 to 3 minutes).

library(network)
library(ergm)

sim.results <- whole_network_function(params = coefs, 
tableego = ego_dat$egodist,
formula = formula_egosim, 
initial.network = initial.randnet,
constant = c(1), pos.cols = c(2),
constant.homophily.change = c(3:length(coefs)),
egonet.data = ego_network_data,
max.alter = max.alter,
var.name.degree = var.name.degree,
var.name.characs = var.name.characs, var.name.characs.alter = var.name.characs.alter,
ns = 10,  delta = c(.5), delta1 = .2,
interval = 50000, burnin = 500000,constraints = ~degreedist, sample=TRUE, 
ego.samp.size = 1000, break_ifnot_improved = F, 
max.iter = 3, useparallel = T, num.cores = 4)

names(sim.results)

# The main items of interest here are: 
# summary.iterations=summary statistics on algorithm
# final.solution.params=coefficients at end of algorithm
# final.solution.alpha=alpha (or decay) value at end of algorithm
# final.solution.chisquare=expected chisquare value based on final solution coefficients

######################################################################
# Simulating networks based on results
# We now take the output from above and generate networks that offer the best fit, given
# the coefficients and fit statistics from sim.results. 
# This processes is not strictly necessary, as we could simply take the last network
# from the simulation results and use that; or alternatively simulate from the
# final solution coefficients. The code below simply takes the best set of coefficients
# (allowing for the possibility that slightly different solutions could offer reasonable
# fits), simulates networks based on the coefficients, and then only keeps those
# networks that have a 'good' chi-square value, within some bound of the best
# chi-square value found above. 

# If we want to do this kind of additional analysis, we can use the
# the run_netlist_function function. The function will output a set of
# networks that are within a given bound of the best chisquare value,
# generating networks until we reach the desired number (set via numnets). The main inputs here
# are sim.results (from above), formula (same as above), constraints (same as above)
# chi.values (best chi square value from sim.results), numnets (how many networks do 
# we want to find), and bound (maximum distance from best chi square to keep generated network). 
# Again, we utilize parallel processing to speed things up. 

netlist <- run_netlist_function(sim.results = sim.results, formula = formula_egosim,
numnets = 10, burnin = 100000, interval = 20000, constraints = ~degreedist,
chi.values = sim.results$best.chisquare,
tableego = ego_dat$egodist, max.alter = max.alter, bound = 30, ns = 5, max.iter = 4,
initial.network = sim.results$last.network,
useparallel = T, num.cores = 4, ego.samp.size = 1000, sample = T)

# We can now take a look at the output: 
netlist[[1]] #the coefficients associated with each network, with the last column
# showing the fit statistic associated with that network

# netlist[[2]] #list of networks

first.net <- netlist[[2]][[1]] #First simulated network
first.net

second.net <- netlist[[2]][[2]] #Second simulated network
second.net

# Once we get the list of networks, we can summarize various statistics of interest,
# like cohesion.

# Note that it also useful to see how the generated networks are fitting
# relative to the observed ego network configuration distribution. So, let's
# write a bit of code to make that comparison easier. 

# getting true distribution of ego net configurations
egodist.true <- round(ego_dat$egodist/sum(ego_dat$egodist)*size.net)

# getting distribution of ego network configurations from first network
egodist.simnet <- table(unlist(egodistfunc(first.net,1:size.net,max.alter=max.alter)))

# And now let's put this together in a simple table, for now putting in 0s
# for the simulated data, and filling in the actual data below.  
compare.dat <- data.frame(egodist.true=egodist.true, egodist.simnet = 0)

#Now putting in the simulated data.
compare.dat[names(egodist.simnet), "egodist.simnet"] <- egodist.simnet
compare.dat 

# The fit looks good on the whole. Of course,
# if we are unsatisfied with the fit, we can rerun the algorithm, with
# better starting coefficients, more iterations, and so on. 



