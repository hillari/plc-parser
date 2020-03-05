-- Hillari M Denny
-- CSCE A331 - PLC
-- Assignment 4 - Lua Recursive Descent Parser

-- =====================================================================
-- parseit.lua
-- @ Glen G. Chappell, Hillari M. Denny
-- 03-01-2020
--
-- Requires lexit.lua
-- This program implements a recursive descent parser in Lua. 
--
-- =====================================================================

local parseit = {}  -- Module for parser

local lexit = require "lexit"  -- Importing our lexer for use in parsing


-- Variables

-- For lexer iteration
local iter          -- Iterator returned by lexit.lex
-- local state         -- State for above iterator (maybe not used)
local lexer_out_s   -- Return value #1 (lexstr) from above iterator
local lexer_out_c   -- Return value #2 (category) from above iterator

-- For current lexeme
local lexstr = ""   -- String form of current lexeme
local lexcat = 0    -- Category of current lexeme:
                    --  one of categories below, or 0 for past the end


-- Symbolic Constants for AST

local STMT_LIST   = 1
local PRINT_STMT  = 2
local FUNC_DEF    = 3
local FUNC_CALL   = 4
local IF_STMT     = 5
local WHILE_STMT  = 6
local RETURN_STMT = 7
local ASSN_STMT   = 8
local STRLIT_OUT  = 9
local CHAR_CALL   = 10
local BIN_OP      = 11
local UN_OP       = 12
local NUMLIT_VAL  = 13
local BOOLLIT_VAL = 14
local INPUT_CALL  = 15
local SIMPLE_VAR  = 16
local ARRAY_VAR   = 17

-- Utility Functions

-- advance
-- Go to next lexeme and load it into lexstr, lexcat.
-- Should be called once before any parsing is done.
-- Function init must be called before this function is called.
local function advance()
    -- Advance the iterator
    lexer_out_s, lexer_out_c = iter(state, lexer_out_s)

    -- If we're not past the end, copy current lexeme into vars
    if lexer_out_s ~= nil then
        lexstr, lexcat = lexer_out_s, lexer_out_c
    else
        lexstr, lexcat = "", 0
    end
end


-- init
-- Initial call. Sets input for parsing functions.
local function init(prog)
    iter, state, lexer_out_s = lexit.lex(prog)
    advance()
end


-- atEnd
-- Return true if pos has reached end of input.
-- Function init must be called before this function is called.
local function atEnd()
    return lexcat == 0
end


-- matchString
-- Given string, see if current lexeme string form is equal to it. If
-- so, then advance to next lexeme & return true. If not, then do not
-- advance, return false.
-- Function init must be called before this function is called.
local function matchString(s)
    if lexstr == s then
        advance()
        return true
    else
        return false
    end
end


-- matchCat
-- Given lexeme category (integer), see if current lexeme category is
-- equal to it. If so, then advance to next lexeme & return true. If
-- not, then do not advance, return false.
-- Function init must be called before this function is called.
local function matchCat(c)
    if lexcat == c then
        advance()
        return true
    else
        return false
    end
end



-------------- Primary Function for Client Code -------------------

-- "local" statements for parsing functions
local parse_expr
local parse_term
local parse_factor

local parse_program
local parse_stmt_list
local parse_statement

-- parse
-- Given program, initialize parser and call parsing function for start
-- symbol. Returns pair of booleans & AST. First boolean indicates
-- successful parse or not. Second boolean indicates whether the parser
-- reached the end of the input or not. AST is only valid if first
-- boolean is true.
function parseit.parse(prog)
    -- Initialization
    init(prog)

    -- Get results from parsing
    local good, ast = parse_program()  -- Parse start symbol
    local done = atEnd()

    -- And return them
    return good, done, ast
end


-- Parsing Functions

-- Each of the following is a parsing function for a nonterminal in the
-- grammar. Each function parses the nonterminal in its name and returns
-- a pair: boolean, AST. On a successul parse, the boolean is true, the
-- AST is valid, and the current lexeme is just past the end of the
-- string the nonterminal expanded into. Otherwise, the boolean is
-- false, the AST is not valid, and no guarantees are made about the
-- current lexeme. 

-- NOTE. Declare parsing functions "local" above, but not below. This
-- allows them to be called before their definitions.

-- parse_program
-- Parsing function for nonterminal "program".
-- Function init must be called before this function is called.
function parse_program()
    local good, ast

    good, ast = parse_stmt_list()
    return good, ast
end


-- parse_stmt_list
-- Parsing function for nonterminal "stmt_list".
-- Function init must be called before this function is called.
function parse_stmt_list()
    local good, ast, newast

    ast = { STMT_LIST }
    while true do  -- this is how we know stmt list is done
        if lexstr ~= "print"
          and lexstr ~= "func"
          and lexstr ~= "if"
          and lexstr ~= "while"
          and lexstr ~= "return" 
          and lexcat ~= lexit.ID then
            return true, ast
        end

        good, newast = parse_statement()
        if not good then
            return false, nil
        end

        table.insert(ast, newast)
    end
end

function parse_statement()
    local good, ast1, ast1

    if matchString("print") then
        if not matchString("(") then
            return false, nil
        end

        if matchString(")") then
            return true, { PRINT_STMT } -- we know we have a valid print statement now, add to list
        end

        good, ast1 = parse_print_arg()  -- now we need to parse the print argument
        if not good then
            return false, nil
        end

        ast2 = { PRINT_STMT, ast1 }

        while matchString(",") do
            good, ast1 = parse_print_arg()
            if not good then
                return false, nil
            end

            table.insert(ast2, ast1)
        end

        if not matchString(")") then -- ?? why do we have this here??
            return false, nil
        end
        
        return true, ast2         

    end -- end print statement logic 


end


function parse_print_arg()
    local good, ast, savelex


    savelex = lexstr
    if matchCat(lexit.STRLIT) then 
        return true, { STRLIT_OUT, savelex }  -- Print argument, string literal
    else
        return false, nil
    end



end

-- parse_expr
-- Parsing function for nonterminal "expr".
-- Function init must be called before this function is called.
function parse_expr()
    local good, ast, saveop, newast

    good, ast = parse_term()
    if not good then
        return false, nil
    end

    while true do
        saveop = lexstr
        if not matchString("+") and not matchString("-") then
            break
        end

        good, newast = parse_term()
        if not good then
            return false, nil
        end

        ast = { { BIN_OP, saveop }, ast, newast }
    end

    return true, ast
end


-- parse_term
-- Parsing function for nonterminal "term".
-- Function init must be called before this function is called.
function parse_term()
    local good, ast, saveop, newast

    good, ast = parse_factor()
    if not good then
        return false, nil
    end

    while true do
        saveop = lexstr
        if not matchString("*") and not matchString("/") then
            break
        end

        good, newast = parse_factor()
        if not good then
            return false, nil
        end

        ast = { { BIN_OP, saveop }, ast, newast }
    end

    return true, ast
end


-- parse_factor
-- Parsing function for nonterminal "factor".
-- Function init must be called before this function is called.
function parse_factor()
    local savelex, good, ast

    savelex = lexstr -- because matchCat advances and we don't want to 'lose' the current lexeme
    if matchCat(lexit.ID) then
        return true, { SIMPLE_VAR, savelex }
    elseif matchCat(lexit.NUMLIT) then
        return true, { NUMLIT_VAL, savelex }
    elseif matchString("(") then
        good, ast = parse_expr()
        if not good then
            return false, nil
        end

        if not matchString(")") then
            return false, nil
        end

        return true, ast
    else
        return false, nil
    end
end



return parseit  -- Export our module 