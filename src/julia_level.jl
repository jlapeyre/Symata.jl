#module SJulia

# This is for testing using SJulia code from Julia
# There is not much done here.


# why cant import apprules ?
#import Main: apprules
#import Mxprs: Mxpr, mxpr
#export Expand

function Expand(mx::Mxpr)
    apprules(mxpr(:Expand,mx))
end

Expand(x) = x

#end   # Module

#############################

#import SJulia.Expand
