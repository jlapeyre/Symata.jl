# Some comaptibility things.

# as of at least
# v"0.5.0-dev+3868"
# 'symbol' is deprecated in favor of 'Symbol'
# The compatibility code here works at least as far back as
# v"0.4.6-pre+7"
const have_new_Symbol =
    try
        Symbol("a","b")
        true
    catch
        false
    end

if ! have_new_Symbol
    Symbol(args...) = symbol(args...)
end
