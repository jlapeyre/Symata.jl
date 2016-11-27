# formatting specification

# formatting specification language
#
#  spec  ::= [[fill]align][sign][#][0][width][,][.prec][type]
#  fill  ::= <any character>
#  align ::= '<' | '>'
#  sign  ::= '+' | '-' | ' '
#  width ::= <integer>
#  prec  ::= <integer>
#  type  ::= 'b' | 'c' | 'd' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G' |
#            'n' | 'o' | 'x' | 'X' | 's'
#
# Please refer to http://docs.python.org/2/library/string.html#formatspec
# for more details
#

## FormatSpec type

const _numtypchars = Set(['b', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X'])

_tycls(c::Char) =
    (c == 'd' || c == 'n' || c == 'b' || c == 'o' || c == 'x') ? 'i' :
    (c == 'e' || c == 'f' || c == 'g') ? 'f' :
    (c == 'c') ? 'c' :
    (c == 's') ? 's' :
    error("Invalid type char $(c)")

immutable FormatSpec
    cls::Char    # category: 'i' | 'f' | 'c' | 's'
    typ::Char
    fill::Char
    align::Char
    sign::Char
    width::Int
    prec::Int
    ipre::Bool   # whether to prefix 0b, 0o, or 0x
    zpad::Bool   # whether to do zero-padding
    tsep::Bool   # whether to use thousand-separator

    function FormatSpec(typ::Char;
               fill::Char=' ',
               align::Char='\0',
               sign::Char='-',
               width::Int=-1,
               prec::Int=-1,
               ipre::Bool=false,
               zpad::Bool=false,
               tsep::Bool=false)

        if align=='\0'
            align = (typ in _numtypchars) ? '>' : '<'
        end
        cls = _tycls(lowercase(typ))
        if cls == 'f' && prec < 0
            prec = 6
        end
        new(cls, typ, fill, align, sign, width, prec, ipre, zpad, tsep)
    end
end

function show(io::IO, fs::FormatSpec)
    println(io, "$(typeof(fs))")
    println(io, "  cls   = $(fs.cls)")
    println(io, "  typ   = $(fs.typ)")
    println(io, "  fill  = $(fs.fill)")
    println(io, "  align = $(fs.align)")
    println(io, "  sign  = $(fs.sign)")
    println(io, "  width = $(fs.width)")
    println(io, "  prec  = $(fs.prec)")
    println(io, "  ipre  = $(fs.ipre)")
    println(io, "  zpad  = $(fs.zpad)")
    println(io, "  tsep  = $(fs.tsep)")
end

## parse FormatSpec from a string

const _spec_regex = r"^(.?[<>])?([ +-])?(#)?(\d+)?(,)?(.\d+)?([bcdeEfFgGnosxX])?$"

function FormatSpec(s::AbstractString)
    # default spec
    _fill = ' '
    _align = '\0'
    _sign = '-'
    _width = -1
    _prec = -1
    _ipre = false
    _zpad = false
    _tsep = false
    _typ = 's'

    if !isempty(s)
        m = match(_spec_regex, s)
        if m == nothing
            error("Invalid formatting spec: $(s)")
        end
        (a1, a2, a3, a4, a5, a6, a7) = m.captures

        # a1: [[fill]align]
        if a1 != nothing
            if length(a1) == 1
                _align = a1[1]
            else
                _fill = a1[1]
                _align = a1[2]
            end
        end

        # a2: [sign]
        if a2 != nothing
            _sign = a2[1]
        end

        # a3: [#]
        if a3 != nothing
            _ipre = true
        end

        # a4: [0][width]
        if a4 != nothing
            if a4[1] == '0'
                _zpad = true
                if length(a4) > 1
                    _width = parse(Int,a4[2:end])
                end
            else
                _width = parse(Int,a4)
            end
        end

        # a5: [,]
        if a5 != nothing
            _tsep = true
        end

        # a6 [.prec]
        if a6 != nothing
            _prec = parse(Int,a6[2:end])
        end

        # a7: [type]
        if a7 != nothing
            _typ = a7[1]
        end
    end

    return FormatSpec(_typ;
                      fill=_fill,
                      align=_align,
                      sign=_sign,
                      width=_width,
                      prec=_prec,
                      ipre=_ipre,
                      zpad=_zpad,
                      tsep=_tsep)
end


## formatted printing using a format spec

type _Dec end
type _Oct end
type _Hex end
type _HEX end
type _Bin end

_srepr(x) = repr(x)
_srepr(x::AbstractString) = x
_srepr(x::Char) = string(x)

if isdefined(:Enum)
    _srepr(x::Enum) = string(x)
end

function printfmt(io::IO, fs::FormatSpec, x)
    cls = fs.cls
    ty = fs.typ
    if cls == 'i'
        ix = @compat Integer(x)
        ty == 'd' || ty == 'n' ? _pfmt_i(io, fs, ix, _Dec()) :
        ty == 'x' ? _pfmt_i(io, fs, ix, _Hex()) :
        ty == 'X' ? _pfmt_i(io, fs, ix, _HEX()) :
        ty == 'o' ? _pfmt_i(io, fs, ix, _Oct()) :
        _pfmt_i(io, fs, ix, _Bin())
    elseif cls == 'f'
        fx = float(x)
        if isfinite(fx)
            ty == 'f' || ty == 'F' ? _pfmt_f(io, fs, fx) :
            ty == 'e' || ty == 'E' ? _pfmt_e(io, fs, fx) :
            error("format for type g or G is not supported yet (use f or e instead).")
        else
            _pfmt_specialf(io, fs, fx)
        end
    elseif cls == 's'
        _pfmt_s(io, fs, _srepr(x))
    else # cls == 'c'
        _pfmt_s(io, fs, @compat Char(x))
    end
end

printfmt(fs::FormatSpec, x) = printfmt(STDOUT, fs, x)

fmt(fs::FormatSpec, x) = sprint(printfmt, fs, x)
fmt(spec::AbstractString, x) = fmt(FormatSpec(spec), x)
