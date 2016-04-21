#module SJulia

# This is for testing using SJulia code from Julia
# There is not much done here.


# why cant import apprules ?
#import Main: apprules
#import Mxprs: Mxpr, mxpr
#export Expand

Expand(mx::Mxpr) = apprules(mxpr(:Expand,mx))
Expand(x) = x

export Expand

#end   # Module

#############################

#import SJulia.Expand
