module SJulia

# why cant import apprules ?
import Main: Mxpr, apprules, mxpr
export Expand

function Expand(mx::Mxpr)
    Main.apprules(mxpr(:Expand,mx))
end

Expand(x) = x

end   # Module

#############################

import SJulia.Expand

