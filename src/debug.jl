## To control debugging information, set these constants and recompile

# debug level, larger means more verbose. -1 is off.
const MXDEBUGLEVEL = -1

# if true, print filenames as they are included.
const VERBOSEINCLUDE = false

# Print symbol names ("functions") as the dispatch code is written via @doap
const VERBOSE_DOAP = false

const PYDEBUGLEVEL = -1

const SJDEBUGLEVEL = -1

###

macro mdebug(level, a...)
    if level <= MXDEBUGLEVEL
        esc(:((println($(a...)));println()))
    else
        nothing
    end
end

macro inc(filename)
    if VERBOSEINCLUDE
        :(println("--- loading ", $filename); include($filename))
    else
        :(include($filename))
    end
end
