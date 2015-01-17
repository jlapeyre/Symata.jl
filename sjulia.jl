## Code to be used with SJulia

function set_symbol_self_eval()
    ccall((:jl_set_symbol_evals_to_self, "libjulia.so"), Void, ())
end

function unset_symbol_self_eval()
    ccall((:jl_unset_symbol_evals_to_self, "libjulia.so"), Void, ())
end

function is_symbol_self_eval()
    ccall((:jl_is_meval_hook, "libjulia.so"), Bool, ())
end

function set_meval_hook()
    ccall((:jl_set_meval_hook, "libjulia.so"), Void, ())
end

function unset_meval_hook()
    ccall((:jl_unset_meval_hook, "libjulia.so"), Void, ())
end

function is_meval_hook()
    ccall((:jl_is_meval_hook, "libjulia.so"), Bool, ())
end

function sjulia_on()
    set_symbol_self_eval()
    set_meval_hook()
    Base.show_quotes_on_symbols(false)    
end

function sjulia_off()
    unset_symbol_self_eval()
    unset_meval_hook()
    Base.show_quotes_on_symbols(true)
end

## functions to work with modified src/interpreter.c

# Call with unbound Symbol as function  creates an Mxpr
Base.call(f::Symbol,args...) = mxpr(f,args...)
Base.call(sym::Symbol) = sym
