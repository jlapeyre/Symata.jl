import Base: LineEdit, REPL

# Code modified from Cxx.jl

global RunSJuliaREPL
function RunSJuliaREPL()

    # Setup sjulia sjulia_prompt
    sjulia_prompt =
        LineEdit.Prompt("sjulia > ";
          # Copy colors from the prompt object
                        prompt_prefix=Base.text_colors[:blue])
    #         prompt_prefix="\e[38;5;166m",  # brownish
    #                        prompt_suffix=Base.text_colors[:black]) # input text color, but we don't need to chane it

    repl = Base.active_repl
    
    sjulia_prompt.on_done =
        REPL.respond(
              (line)->(Base.parse_input_line("@SJulia.ex $line")),
               repl, sjulia_prompt) # stay in symjulia

#        sjulia_prompt.on_done = REPL.respond(repl,sjulia_prompt) do line
#        SJulia.process_sjulia_string(string(line,"\n;"), isTopLevelExpression(C,line), :REPL, 1, 1;
#                compiler = C)
#        end

    main_mode = repl.interface.modes[1]
    
    push!(repl.interface.modes,sjulia_prompt)
    
    hp = main_mode.hist
    hp.mode_mapping[:sjulia] = sjulia_prompt
    sjulia_prompt.hist = hp

    const enter_sjulia_key = '='
    const sjulia_keymap = Dict{Any,Any}(
           enter_sjulia_key => function (s,args...)
            if isempty(s)
                if !haskey(s.mode_state,sjulia_prompt)
                    s.mode_state[sjulia_prompt] = LineEdit.init_state(repl.t,sjulia_prompt)
                end
                LineEdit.transition(s,sjulia_prompt)
            else
                LineEdit.edit_insert(s,enter_sjulia_key)
            end
           end
     )

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    mk = REPL.mode_keymap(main_mode)

    b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    sjulia_prompt.keymap_dict = LineEdit.keymap(b)
    
    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, sjulia_keymap);
    nothing
end
